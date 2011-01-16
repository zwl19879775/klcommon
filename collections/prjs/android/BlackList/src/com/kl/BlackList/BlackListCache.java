package com.kl.BlackList;

import java.util.ArrayList;
import java.util.List;

import android.content.AsyncQueryHandler;
import android.content.ContentResolver;
import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;

import com.kl.android.BlackListProvider.BlackList;

public class BlackListCache {
	public static final int ID_IDX = 0;
	public static final int ADDRESS_IDX = 1;
	public static final int DATE_IDX = 2;
	public static final int LATESTLOG_IDX = 3;
	public static final int LOGCOUNT_IDX = 4;
	public static final String[] PROJECTION = {
		BlackList.ID,
		BlackList.ADDRESS,
		BlackList.DATE,
		BlackList.LATEST_LOG,
		BlackList.LOG_COUNT
	};
	private static final int TOK_QUERY = 1;
	private static final int TOK_INSERT = 2;
	private static final int TOK_QUERY_INSERTED = 3;
	private static final int TOK_DELETE_ONE = 4;
	private static final int TOK_DELETE_ALL = 5;
	public static final int IDALL = 0;
	
	public final static class Item {
		long id;
		String address;
		long date;
		long logCount;
		String latestLog;
	}
	public interface OnOperCompleted{
		void onCompleted();
		void onInserted(Item item);
	}
	
	private class Querier extends AsyncQueryHandler {
		public Querier(ContentResolver cr) {
			super(cr);
		}
		
		@Override
		protected void onQueryComplete(int token, Object cookie, Cursor cursor) {
			Log.d("Async query BlackList completed.");
			if(token == TOK_QUERY) {
				loadAll(cursor, (OnOperCompleted) cookie);
			}
			else if(token == TOK_QUERY_INSERTED) {
				loadInserted(cursor, (OnOperCompleted) cookie);
			}
		}
		
		@Override
		protected void onInsertComplete(int token, Object cookie, Uri uri) {
			Log.d("Async insert BlackList completed.");
			onInsertCompleted((OnOperCompleted) cookie, uri);
		}
		
		@Override
		protected void onDeleteComplete(int token, Object cookie, int ret) {
			Log.d("Async delete BlackList completed.");
			OnOperCompleted h = (OnOperCompleted) cookie;
			h.onCompleted();
		}
	}	
	
	private Querier mQuerier;
	private List<Item> mCache;
	private Context mContext;
	private boolean mLatest;
	private boolean mNotify;
	private static BlackListCache mInst;
	
	public BlackListCache(Context context) {
		mContext = context;
		mCache = new ArrayList<Item>();
		mQuerier = new Querier(mContext.getContentResolver());
		mLatest = false;
		mNotify = false;
	}

	public static void init(Context context) {
		Log.i("BlackListCache init.");
		mInst = new BlackListCache(context);
	}

	public static BlackListCache inst() {
		return mInst;
	}

	public List<Item> getCache() {
		return mCache;
	}
	
	public Item get(int pos) {
		return mCache.get(pos);
	}
	
	public int size() {
		return mCache.size();
	}
	
	public Item get(String address) {
		for(Item item : mCache) {
			if(item.address.equals(address)) return item;
		}
		return null;
	}
	
	private boolean remove(long id) {
		for(Item item : mCache) {
			if(id == item.id) {
				mCache.remove(item);
				return true;
			}
		}
		return false;
	}
	
	public void startAsyncQuery(OnOperCompleted handler) {
		Log.i("Start to query all black list.");
		if(!mLatest) {
			mQuerier.startQuery(TOK_QUERY, handler, BlackList.CONTENT_URI, PROJECTION, 
					null, null, null); 
		}
		else if(mNotify){
			Log.i("The black list is latest, use cache.");
			handler.onCompleted();
			mNotify = false;
		}
	}
	
	public void startAsyncInsert(OnOperCompleted handler, String address) {
		Item existItem = get(address);
		if(existItem != null) {
			Log.d("Black list item already exists:" + address);
			return;
		}
		ContentValues val = new ContentValues();
		val.put(BlackList.ADDRESS, address);
		mQuerier.startInsert(TOK_INSERT, handler, BlackList.CONTENT_URI, val);
	}

	public void startAsyncDelete(OnOperCompleted handler, long id) {
		if(id == IDALL) {
			mQuerier.startDelete(TOK_DELETE_ALL, handler, BlackList.CONTENT_URI, null, null);
			mCache.clear();
		}
		else {
			Uri uri = ContentUris.withAppendedId(BlackList.CONTENT_URI, id);
			mQuerier.startDelete(TOK_DELETE_ONE, handler, uri, null, null);
			remove(id);
		}
	}
	
	private void loadAll(Cursor cursor, OnOperCompleted handler) {
		if(!cursor.moveToFirst()) {
			return;
		}
		do {
			Item item = loadOne(cursor);
			mCache.add(item);
		} while(cursor.moveToNext());
		handler.onCompleted();
		mLatest = true;
		Log.i(String.format("Read %d black list items.", mCache.size()));
	}

	private Item loadOne(Cursor cursor) {
		Item item = new Item();
		item.id = cursor.getLong(ID_IDX);
		item.date = cursor.getLong(DATE_IDX);
		item.address = cursor.getString(ADDRESS_IDX);
		item.logCount = cursor.getLong(LOGCOUNT_IDX);
		item.latestLog = cursor.getString(LATESTLOG_IDX);
		Log.d(String.format("load one black list (%s).", item.address));
		return item;
	}	
	
	private void loadInserted(Cursor cursor, OnOperCompleted handler) {
		if(!cursor.moveToFirst()) {
			Log.e("Insert black list item failed.");
			return;
		}
		Item item = loadOne(cursor);
		if(item != null) {
			Log.d("Query inserted item completed, cache it.");
			mCache.add(item);
			handler.onInserted(item);
			mNotify = true;
		}
	}
	
	private void onInsertCompleted(OnOperCompleted handler, Uri uri) {
		handler.onCompleted();
		Log.d("Start query inserted item.");
		mQuerier.startQuery(TOK_QUERY_INSERTED, handler, uri, PROJECTION, null, null, null);
	}
}
