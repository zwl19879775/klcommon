package com.kl.BlackList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import android.content.AsyncQueryHandler;
import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;

public class Conversation {
	public static final String TAG = "Conversation";
	private static final Uri FULL_CONTENT_URI = Uri.parse("content://mms-sms/conversations");
	private static final Uri CONTENT_URI = FULL_CONTENT_URI .buildUpon().
		appendQueryParameter("simple", "true").build();
    private static final String ID = "_id";
    private static final String DATE = "date";
    private static final String MESSAGE_COUNT = "message_count";
    private static final String RECIPIENT_IDS = "recipient_ids";
    private static final String SNIPPET = "snippet";
    private static final int ID_INDEX = 0;
    private static final int DATE_INDEX = 1;
    private static final int MESSAGE_COUNT_INDEX = 2;
    private static final int SNIPPET_INDEX = 3;
    private static final int RECIPIENT_IDS_INDEX = 4;
    private static final String[] PROJECTION = { 
    	ID, DATE, MESSAGE_COUNT, SNIPPET, RECIPIENT_IDS }; 
    
    public final class Item {
    	long id;
    	long date;
    	long msgCnt;
    	String recipientIds;
    	String snippet;
    }
    
    public interface OnQueryCompleted {
    	void onCompleted(boolean success);
    }
    
    public interface OnTraverse {
    	void onTraverse(Item item);
    }
    
	private class Querier extends AsyncQueryHandler {
		public Querier(ContentResolver cr) {
			super(cr);
		}
		
		@Override
		protected void onQueryComplete(int token, Object cookie, Cursor cursor) {
			Log.d("Async query completed.");
			readAll(cursor, (OnQueryCompleted) cookie);
		}
	}
	
	private Querier mQuerier;
	private Context mContext;
	private Map<Long, Item> mCache;
	private static Conversation mInst;
	
	public Conversation(Context context) {
		mContext = context;
		mCache = new HashMap<Long, Item>();
		mQuerier = new Querier(mContext.getContentResolver());
	}
	
	public static void init(Context context) {
		Log.i("Conversation init.");
		mInst = new Conversation(context);
	}
	
	public static Conversation inst() {
		return mInst;
	}
	
	public void startAsyncQuery(OnQueryCompleted handler) {
		mQuerier.startQuery(0, handler, CONTENT_URI, PROJECTION, null, null, null);
	}
	
	private void readAll(Cursor cursor, OnQueryCompleted handler) {
		if(!cursor.moveToFirst()) {
			handler.onCompleted(false);
			return;
		}
		do {
			Item item = readOne(cursor);
			mCache.put(item.id, item);
		} while(cursor.moveToNext());
		handler.onCompleted(true);
		Log.i(String.format("Read %d items.", mCache.size()));
	}
	
	private Item readOne(Cursor cursor) {
		Item item = new Item();
		item.id = cursor.getLong(ID_INDEX);
		item.date = cursor.getLong(DATE_INDEX);
		item.msgCnt = cursor.getLong(MESSAGE_COUNT_INDEX);
		item.recipientIds = cursor.getString(RECIPIENT_IDS_INDEX);
		item.snippet = cursor.getString(SNIPPET_INDEX);
		return item;
	}
	
	public void tranverse( OnTraverse t, Comparator<Item> comp ) {
		Collection<Item> vals = mCache.values();
		ArrayList<Item> items = new ArrayList<Item>( vals );
		if(comp != null) {
			Collections.sort(items, comp);
		}
		int size = items.size();
		for( int i = 0; i < size; ++ i ) {
			t.onTraverse(items.get(i));
		}
	}
	
	public Item get(long id) {
		return mCache.get(id);
	}
}
