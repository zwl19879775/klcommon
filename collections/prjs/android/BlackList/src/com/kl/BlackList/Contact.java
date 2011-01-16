package com.kl.BlackList;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.content.AsyncQueryHandler;
import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.database.CursorIndexOutOfBoundsException;
import android.net.Uri;
import android.provider.ContactsContract;

public class Contact {
	private static final int QUERY_ONE = 1;
	private static final int QUERY_ALL = 2;
	private static final int QUERY_PHONES = 3;
	private static final String[] CONTACT_PROJECTION = {
		ContactsContract.Contacts._ID,
		ContactsContract.Contacts.DISPLAY_NAME, 
		ContactsContract.Contacts.HAS_PHONE_NUMBER };
	private static final int ID_INDEX = 0;
	private static final int DISPLAY_NAME_INDEX = 1;
	private static final int HAS_PHONE_NUMBER_INDEX = 2;
	
    public interface OnLoadPhone {
    	void onLoad(Item contact, Phone phone);
    }
    
    public interface OnTraverse {
    	void onTraverse(Item item);
    }	
    
	public final class Phone {
		String number;
		long type;
		
		public Phone(String number, long t) {
			this.number = number;
			type = t;
		}
	}
	
	public interface OnItemUpdate {
		void onUpdate(Item item);
	}
	
	public final class Item {
		long id;
		String displayName;
		List<Phone> phones;
		boolean full = false; /// all data has got if true.
		List<OnItemUpdate> mListeners = new ArrayList<OnItemUpdate>();
		
		public Item(String number) {
			displayName = number;
			phones = new ArrayList<Phone>();
			phones.add(new Phone(number, 0));
		}
		
		public Item() {
			displayName = "";
			phones = new ArrayList<Phone>();
		}
		
		public int phoneCount() {
			return phones != null ? phones.size() : 0;
		}
		
		public boolean hasPhone(String number) {
			for(Phone phone : phones) {
				if(phone.number == number) return true;
			}
			return false;
		}
		
		public void addListener(OnItemUpdate listener) {
			mListeners.add(listener);
		}
		
		public void removeListener(OnItemUpdate listener) {
			mListeners.remove(listener);
		}
		
		public void notifyUpdate() {
			for(OnItemUpdate listener : mListeners) {
				listener.onUpdate(this);
			}
		}
	}
	
	private class Querier extends AsyncQueryHandler {
		public Querier(ContentResolver cr) {
			super(cr);
		}
		
		@Override
		protected void onQueryComplete(int token, Object cookie, Cursor cursor) {
			if(token == QUERY_ONE) {
				loadOne((Item) cookie, cursor, true);
			}
			else if(token == QUERY_ALL) {
				loadAll(cursor);
			}
			else if(token == QUERY_PHONES) {
				loadPhone((Item) cookie, cursor);
			}
		}
	}
	
	private static Contact mInst;
	private Context mContext;
	private Querier mQuerier;
	/// <phone_number, item>
	private Map<String, Item> mCache;
	private OnLoadPhone mPhoneObserver;
	
	public Contact(Context context) {
		mContext = context;
		mQuerier = new Querier(mContext.getContentResolver());
		mCache = new HashMap<String, Item>();
	}
	
	public static void init(Context context) {
		Log.i("Contact init.");
		mInst = new Contact(context);
	}
	
	public static Contact inst() {
		return mInst;
	}
	
	public Item get(String number) {
		if(mCache.containsKey(number)) {
			return mCache.get(number);
		}
		Item item = new Item(number);
		mCache.put(number, item);
		return item;
	}
	
	public void setPhoneLoadObserver(OnLoadPhone ob) {
		mPhoneObserver = ob;
	}
	
	public Item startAsyncQuery(String number) {
		Item item = get(number);
		if(item.full) return item;
		Uri personUri = Uri.withAppendedPath(
				ContactsContract.PhoneLookup.CONTENT_FILTER_URI, number);		
		mQuerier.startQuery(QUERY_ONE, item, personUri, CONTACT_PROJECTION, null, null, null);
		Log.d(String.format("Start async query contact:%s", number));
		return item;
	}
	
	public void startAsyncQuery() {
		Log.d("Start to query all contacts.");
		mQuerier.startQuery(QUERY_ALL, null, ContactsContract.Contacts.CONTENT_URI,
				CONTACT_PROJECTION, null, null, null);
	}
	
	private void startQueryPhones(Item item) {
		Log.d("Start async query phones:" + item.displayName);
		mQuerier.startQuery(QUERY_PHONES, item,
				ContactsContract.CommonDataKinds.Phone.CONTENT_URI, 
				null, ContactsContract.CommonDataKinds.Phone.CONTACT_ID + "=?",
				new String[] { String.valueOf(item.id)}, null);	
	}
	
	private void loadOne(Item item, Cursor cursor, boolean single) {
		if(single && !cursor.moveToFirst()) {
			Log.w("Query contact failed:" + item.displayName);
			item.full = true;
			return;
		}
		long phoneNum = 0;
		try {
			item.id = cursor.getLong(ID_INDEX);
			item.displayName = cursor.getString(DISPLAY_NAME_INDEX);
			Log.d("Query contact finished:" + item.displayName);
			item.notifyUpdate();
			phoneNum = cursor.getLong(HAS_PHONE_NUMBER_INDEX);
		}
		catch(CursorIndexOutOfBoundsException e) {
			Log.e("CursorOutOfBoundsException on:" + item.displayName);
			return;
		}
		if(phoneNum <= 0) {
			item.full = true;
			Log.d("No phone number:" + item.displayName);
		}
		else {
			startQueryPhones(item);
		}
	}
	
	private void loadAll(Cursor cursor) {
		if(!cursor.moveToFirst()) {
			Log.w("Query all contact failed.");
			return;
		}
		do {
			// If the contact has phone number, then it will be put in the cache.
			// Otherwise, it will be GC soon.
			Item item = new Item();
			loadOne(item, cursor, false);
		} while(cursor.moveToNext());
	}
	
	private void loadPhone(Item item, Cursor cursor) {
		if(!cursor.moveToFirst()) {
			Log.w("Query phones failed:" + item.displayName);
			return;
		}
		item.phones.clear();
		int phoneIdx = cursor.getColumnIndex(ContactsContract.CommonDataKinds.Phone.NUMBER);
		int typeIdx = cursor.getColumnIndex(ContactsContract.CommonDataKinds.Phone.TYPE);
		do {
			Phone phone = new Phone(cursor.getString(phoneIdx),
					cursor.getInt(typeIdx));
			item.phones.add(phone);
			hashByNumber(item, phone.number);
			// notify listener.
			if(mPhoneObserver != null) {
				mPhoneObserver.onLoad(item, phone);
			}
		}while(cursor.moveToNext());	
		// mark full.
		item.full = true;
		// notify.
		item.notifyUpdate();
		Log.d("Query phones completed:" + item.displayName);
	}
	
	private void hashByNumber(Item item, String number) {
		mCache.put(number, item);
	}
}
