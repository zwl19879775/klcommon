/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.test.ConversationTest;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import android.content.Context;
import android.database.ContentObserver;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Handler;
import android.provider.ContactsContract;

/// Observe on contact list database.
class ContactObserver extends ContentObserver {
	private Context mContext;
	
	public ContactObserver( Context context, Handler h ) {
		super(h);
		mContext = context;
	}
	
	@Override
	public void onChange( boolean selfChange ) {
		super.onChange(selfChange);
		Log.d( "ContactObserver:onChange");
		ContactManager.instance().query(mContext, 0);
	}
	
	public static void register(Context context) {
		Log.d("ContactObserver:register");
		ContactObserver ob = new ContactObserver( context, new Handler() );
		context.getContentResolver().registerContentObserver(ContactsContract.Contacts.CONTENT_URI,
				true, ob);
	}
}

/// Global manager to get contact list.
public class ContactManager {
	/// Phone number.
	public class PhoneNum {
		String mNumber;
		int mType;
	}
	
	public interface OnUpdated {
		void onUpdate(Contact contact);
	}
	
	/// A contact item.
	public class Contact {
		public long mID = -1;
		public long mLastTimeContacted;
		public String mName;
		public List<PhoneNum> mPhones = new ArrayList<PhoneNum>();
		private byte[] mAvatarData;
		private Drawable mAvatar;
		private OnUpdated mListener;
		
		public int phoneCount() {
			return mPhones.size();
		}
		
		public boolean valid() {
			return mID > 0;
		}
		
		public void setListener(OnUpdated listener) {
			mListener = listener;
		}
		
		public void notifyUpdate() {
			if(mListener != null) mListener.onUpdate(this);
		}
		
		/// Direct get MOBILE phone number.
		public String mobilePhone() {
			for(PhoneNum phone : mPhones) {
				if(phone.mType == ContactsContract.CommonDataKinds.Phone.TYPE_MOBILE) {
					return phone.mNumber;
				}
			}
			if(phoneCount() > 0) return mPhones.get(0).mNumber;
			return null;
		}
		
		/// Check whether this contact has the phone.
		public boolean hasPhone(String phone) {
			return mPhones.contains(phone);
		}
		
		/// Get avatar icon.
		public Drawable getAvatar(Context context, Drawable def) {
			if(mAvatar == null) {
	            if(mAvatarData != null) {
	                Bitmap b = BitmapFactory.decodeByteArray(mAvatarData, 0, mAvatarData.length);
	                mAvatar = new BitmapDrawable(context.getResources(), b);
	            }
			}
			return mAvatar != null ? mAvatar : def;
		}
	}
	
	/// Used to sort the Contact list by LastContactedTime.
	private class ContactComparator implements Comparator<Contact> {
		public int compare(Contact left, Contact right) {
			if(left.mLastTimeContacted == right.mLastTimeContacted) return 0;
			if(left.mLastTimeContacted < right.mLastTimeContacted ) return -1;
			return 1;
		}
	}
	
	public interface Traverser {
		public void onTraverse(Contact contact);
	}
	
	private static ContactManager mInst = null;
	private TaskStack mTaskStack;
	/// <contactID, contact>
	private HashMap<Long, Contact> mContacts = new HashMap<Long, Contact>();
	/// <phoneNumber, contact>
	private HashMap<String, Contact> mNumber2Contacts = new HashMap<String, Contact>();
	private boolean mQueriedAll = false;
	
	ContactManager() {
		mTaskStack = new TaskStack();
	}
	
	public static ContactManager instance() {
		if(mInst == null) mInst = new ContactManager();
		return mInst;
	}
	
	public int size() {
		return mContacts.size();
	}
	
	public boolean checkQuery(Context context) {
		if(!mQueriedAll) {
			return query(context, 0);
		}
		return true;
	}
	
	/// Get the recent 'count' contacts.
	public void traverse(Traverser tr, int count) {
		Collection<Contact> vals = mContacts.values();
		if(count == 0) { //all
			for(Contact contact : vals) {
				tr.onTraverse(contact);
			}
		}
		else { // some contacted persons.
			ArrayList<Contact> items = new ArrayList<Contact>( vals );
			// sort by lastContactTime.
			Collections.sort(items, new ContactComparator());
			int size = items.size();
			for(int i = 0; i < size && count > 0; --count, ++i) {
				Contact contact = items.get(size - i - 1);
				if(contact.mLastTimeContacted == 0) break;
				tr.onTraverse(contact);
			}
		}
	}
	
	public Contact asyncQueryContact(final Context context, final String address) {
		Contact contact = getFromCache(address);
		if(contact != null) {
			return contact;
		}
		contact = getSimpleContact(address);
		final Contact c = contact;
		Runnable r = new Runnable() {
			public void run() {
				boolean ret = queryContactByAddr(c, context, address);
				if(ret) c.notifyUpdate();
			}
		};
		mTaskStack.push(r);
		return contact;
	}
	
	public boolean queryContactByAddr(Contact contact, Context context, String address) {
		Uri personUri = Uri.withAppendedPath(
				ContactsContract.PhoneLookup.CONTENT_FILTER_URI, address);
		String[] lookup = new String[] { 
				ContactsContract.Contacts._ID,
				ContactsContract.Contacts.DISPLAY_NAME, 
				ContactsContract.Contacts.LAST_TIME_CONTACTED,
				ContactsContract.Contacts.HAS_PHONE_NUMBER,
				ContactsContract.Contacts.PHOTO_ID
		};			
		Cursor cur = context.getContentResolver().query(personUri,
				lookup,
				null, null, null );
		if( !cur.moveToFirst() ) {
			Log.w("Query contact id by address failed:" + address);
			return false;
		}
		int idIdx = cur.getColumnIndex(lookup[0]);
		int nameIdx = cur.getColumnIndex(lookup[1]);
		int lastContactIdx = cur.getColumnIndex(lookup[2]);
		int phoneNumIdx = cur.getColumnIndex(lookup[3]);
		int photoIdx = cur.getColumnIndex(lookup[4]);
		
		long id = cur.getLong(idIdx);
		if(!mContacts.containsKey(id)) {
			contact.mID = cur.getLong(idIdx);
			contact.mName = cur.getString(nameIdx);
			contact.mLastTimeContacted = cur.getLong(lastContactIdx);
			contact.mPhones.clear();
			int cnt = cur.getInt(phoneNumIdx);
			if(cnt > 0) { // has phone number.
				queryPhones(contact, context);
			}
			long photoID = cur.getLong(photoIdx);
			contact.mAvatarData = queryPhoto(contact, context, photoID);
			cache(contact);
		}
		cur.close();
		return true;
	}
	
	/// A stranger, we only have his phone number.
	public Contact getSimpleContact(String number) {
		Contact contact = new Contact();
		contact.mName = number;
		PhoneNum num = new PhoneNum();
		num.mNumber = number;
		contact.mPhones.add(num);
		mNumber2Contacts.put(number, contact);
		return contact;
	}
	
	private void cache(Contact contact) {
		if(contact.valid()) {
			mContacts.put(contact.mID, contact);
		}
		for(PhoneNum num : contact.mPhones) {
			mNumber2Contacts.put(num.mNumber, contact);
		}
	}
	
	public Contact getFromCache(String number) {
		return mNumber2Contacts.get(number);
	}
	
	public boolean query(Context context, long contactID) {
		if(size() > 0) {
			Log.d("Clear the cache contact list.");
			mContacts.clear();
		}
		String[] lookup = new String[] { 
				ContactsContract.Contacts._ID,
				ContactsContract.Contacts.DISPLAY_NAME, 
				ContactsContract.Contacts.LAST_TIME_CONTACTED,
				ContactsContract.Contacts.HAS_PHONE_NUMBER,
				ContactsContract.Contacts.PHOTO_ID
		};			
		String selection = null;
		String[] selArg = null;
		if(contactID > 0) {
			selection = lookup[0] + "=?";
			selArg = new String[] { String.valueOf(contactID) };
		}
		else {
			mQueriedAll = true;
		}
		
		Cursor cur = context.getContentResolver().query(ContactsContract.Contacts.CONTENT_URI, 
				lookup, selection, selArg, null);
		if(!cur.moveToFirst()) {
			Log.w("Query contact list failed.");
			return false;
		}
		int idIdx = cur.getColumnIndex(lookup[0]);
		int nameIdx = cur.getColumnIndex(lookup[1]);
		int lastContactIdx = cur.getColumnIndex(lookup[2]);
		int phoneNumIdx = cur.getColumnIndex(lookup[3]);
		int photoIdx = cur.getColumnIndex(lookup[4]);
		do {
			Contact contact = new Contact();
			contact.mID = cur.getLong(idIdx);
			contact.mName = cur.getString(nameIdx);
			contact.mLastTimeContacted = cur.getLong(lastContactIdx);
			int cnt = cur.getInt(phoneNumIdx);
			if(cnt > 0) { // has phone number.
				queryPhones(contact, context);
			}
			long photoID = cur.getLong(photoIdx);
			contact.mAvatarData = queryPhoto(contact, context, photoID);
			cache(contact);
		} while(cur.moveToNext());
		cur.close();
		return true;
	}
	
	private boolean queryPhones(Contact contact, Context context) {
		Cursor cur = context.getContentResolver().query(ContactsContract.CommonDataKinds.Phone.CONTENT_URI, 
				null, ContactsContract.CommonDataKinds.Phone.CONTACT_ID + "=?" , 
				new String[] { String.valueOf(contact.mID)}, 
				null);
		if(!cur.moveToFirst()) {
			Log.w("Query phones failed:" + contact.mName);
			return false;
		}
		int phoneIdx = cur.getColumnIndex(ContactsContract.CommonDataKinds.Phone.NUMBER);
		int typeIdx = cur.getColumnIndex(ContactsContract.CommonDataKinds.Phone.TYPE);
		do {
			PhoneNum phone = new PhoneNum();
			phone.mNumber = cur.getString(phoneIdx);
			phone.mType = cur.getInt(typeIdx);
			contact.mPhones.add(phone);
			Log.d("Get phone number:" + phone.mNumber);
		}while(cur.moveToNext());
		cur.close();
		return true;
	}
	
	private byte[] queryPhoto(Contact contact, Context context, long photoID) {
		Cursor cur = context.getContentResolver().query(ContactsContract.Data.CONTENT_URI,
				new String[] { ContactsContract.Data.DATA15 },
				ContactsContract.Data._ID + "=?", 
				new String[] { String.valueOf(photoID) }, null);
		if(!cur.moveToFirst()) {
			Log.w("Query photo id failed.");
			return null;
		}
		byte[] photo = cur.getBlob(0);
		return photo;
	}
}
