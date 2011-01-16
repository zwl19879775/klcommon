package com.kl.BlackList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import android.content.AsyncQueryHandler;
import android.content.ContentResolver;
import android.content.Context;
import android.database.ContentObserver;
import android.database.Cursor;
import android.os.Handler;
import com.kl.android.BlackListProvider.BlackList;

public class BlockLog {
	private static final int ID_IDX = 0;
	private static final int BLACKID_IDX = 1;
	private static final int TYPE_IDX = 2;
	private static final int DATE_IDX = 3;
	private static final int DESC_IDX = 4;
	private static final String[] PROJECTION = {
		BlackList.BlockLog.ID,
		BlackList.BlockLog.BLACK_ID,
		BlackList.BlockLog.TYPE,
		BlackList.BlockLog.DATE,
		BlackList.BlockLog.DESC,
	};
	
	public static final class Item  {
		long id;
		long blackId;
		long type;
		long date;
		String desc;
	}
	
	/// Items attached to a black list.
	public static final class BlackLogItem {
		List<Item> logs;
		boolean dirty;
		
		BlackLogItem() {
			logs = new ArrayList<Item>();
			dirty = false;
		}
		
		long blackID() {
			return logs.size() > 0 ? logs.get(0).blackId : 0;
		}
		
		int count() {
			return logs.size();
		}
	}
	
	public interface OnOperCompleted {
		void onCompleted(boolean ret);
	}
	
	private class Querier extends AsyncQueryHandler {
		public Querier(ContentResolver cr) {
			super(cr);
		}
		
		@Override
		protected void onQueryComplete(int token, Object cookie, Cursor cursor) {
			Log.d("Async query block log completed.");
			loadBlackItem((OnOperCompleted) cookie, cursor);
		}
	}	
	
	public class BlockLogObserver extends ContentObserver {
		public BlockLogObserver(Handler h) {
			super(h);
		}
		
		@Override
		public void onChange( boolean selfChange ) {
			super.onChange(selfChange);
			Log.d("BlockLog has changed.");
			markAllDirty();
		}
		
		public void register(Context context) {
			context.getContentResolver().registerContentObserver(BlackList.BlockLog.CONTENT_URI,
					true, this);
		}
	}	
	
	private Querier mQuerier;
	private Map<Long, BlackLogItem> mCache;
	private Context mContext;
	private BlockLogObserver mLogObserver;
	private static BlockLog mInst;
	
	public BlockLog(Context context) {
		mContext = context;
		mCache = new HashMap<Long, BlackLogItem>();
		mQuerier = new Querier(mContext.getContentResolver());
		mLogObserver = new BlockLogObserver(new Handler());
		mLogObserver.register(context);
	}

	public static void init(Context context) {
		Log.i("Block log init.");
		mInst = new BlockLog(context);
	}

	public static BlockLog inst() {
		return mInst;
	}
	
	public BlackLogItem get(long blackID) {
		return mCache.get(blackID);
	}
	
	public void markAllDirty() {
		Collection<BlackLogItem> vals = mCache.values();
		Iterator<BlackLogItem> it = vals.iterator();
		while(it.hasNext()) {
			BlackLogItem logs = it.next();
			logs.dirty = true;
			Log.d(String.format("Mark %d block log dirty.", logs.blackID()));
		}
	}
	
	public void startAsyncQuery(OnOperCompleted handler, long blackID) {
		Log.d("Start to query block log:" + blackID);
		if(checkInCache(handler, blackID)) {
			Log.d("Use block log cache.");
			return ;
		}
		mQuerier.startQuery(0, handler, BlackList.BlockLog.CONTENT_URI, PROJECTION, 
				BlackList.BlockLog.BLACK_ID + "=?", 
				new String[] { String.valueOf(blackID) }, null);
	}
	
	private boolean checkInCache(OnOperCompleted handler, long blackID) {
		BlackLogItem logs = mCache.get(blackID);
		if(logs != null && !logs.dirty) {
			handler.onCompleted(true);
			return true;
		}
		return false;
	}
	
	private void loadBlackItem(OnOperCompleted handler, Cursor cursor) {
		if(!cursor.moveToFirst()) {
			Log.e("Query block log failed.");
			handler.onCompleted(false);
			return;
		}
		BlackLogItem logs = new BlackLogItem();
		do {
			Item log = loadBlockLog(cursor);
			logs.logs.add(log);
		} while(cursor.moveToNext());
		mCache.put(logs.blackID(), logs);
		handler.onCompleted(true);
		Log.d("Load block logs completed.");
	}
	
	private Item loadBlockLog(Cursor cursor) {
		Item log = new Item();
		log.id = cursor.getLong(ID_IDX);
		log.blackId = cursor.getLong(BLACKID_IDX);
		log.date = cursor.getLong(DATE_IDX);
		log.type = cursor.getLong(TYPE_IDX);
		log.desc = cursor.getString(DESC_IDX);
		return log;
	}	
}
