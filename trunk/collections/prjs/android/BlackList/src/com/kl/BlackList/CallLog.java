package com.kl.BlackList;

import java.util.ArrayList;
import java.util.List;
import android.content.AsyncQueryHandler;
import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;

public class CallLog {
	public static final int TYPE_INCOMING = android.provider.CallLog.Calls.INCOMING_TYPE;
	public static final int TYPE_MISSED = android.provider.CallLog.Calls.MISSED_TYPE;
	public static final int TYPE_OUTGOING = android.provider.CallLog.Calls.OUTGOING_TYPE;
	private static final String[] PROJECTION = {
		android.provider.CallLog.Calls.CACHED_NAME,
		android.provider.CallLog.Calls.NUMBER,
		android.provider.CallLog.Calls.TYPE,
		android.provider.CallLog.Calls.DATE,
	};
	private static final int NAME_IDX = 0;
	private static final int NUMBER_IDX = 1;
	private static final int TYPE_IDX = 2;
	private static final int DATE_IDX = 3;
	
	public final class Item {
		String cachedName;
		int type;
		String number;
		long date;
	}
	
	public interface OnQueryCompleted{
		void onCompleted();
		void onOneCompleted(Item item);
	}
	
	private class Querier extends AsyncQueryHandler {
		public Querier(ContentResolver cr) {
			super(cr);
		}
		
		@Override
		protected void onQueryComplete(int token, Object cookie, Cursor cursor) {
			Log.d("Async query CallLog completed.");
			loadAll(cursor, (OnQueryCompleted) cookie);
		}
	}	
	
	private Querier mQuerier;
	private List<Item> mCache;
	private Context mContext;
	private static CallLog mInst;
	
	public CallLog(Context context) {
		mContext = context;
		mCache = new ArrayList<Item>();
		mQuerier = new Querier(mContext.getContentResolver());
	}

	public static void init(Context context) {
		Log.i("CallLog init.");
		mInst = new CallLog(context);
	}

	public static CallLog inst() {
		return mInst;
	}

	public List<Item> getLogs() {
		return mCache;
	}
	
	public Item get(int pos) {
		return mCache.get(pos);
	}
	
	public void startAsyncQuery(OnQueryCompleted handler) {
		Log.i("Start to query call logs.");
		mCache.clear();
		mQuerier.startQuery(0, handler, android.provider.CallLog.Calls.CONTENT_URI, PROJECTION, 
				null, null, "date desc"); // make the latest log to be first.
	}

	private void loadAll(Cursor cursor, OnQueryCompleted handler) {
		if(!cursor.moveToFirst()) {
			return;
		}
		do {
			Item item = loadOne(cursor);
			mCache.add(item);
			handler.onOneCompleted(item);
		} while(cursor.moveToNext());
		handler.onCompleted();
		Log.i(String.format("Read %d call log items.", mCache.size()));
	}

	private Item loadOne(Cursor cursor) {
		Item item = new Item();
		item.cachedName = cursor.getString(NAME_IDX);
		item.date = cursor.getLong(DATE_IDX);
		item.type = cursor.getInt(TYPE_IDX);
		item.number = cursor.getString(NUMBER_IDX);
		if(item.cachedName == null) {
			item.cachedName = item.number;
		}
		Log.d(String.format("load one call log (%s)(%s).", item.cachedName, item.number));
		return item;
	}	
}
