/*
 * @author Kevin Lynx
 * @date 1.3.2010
 * @brief Query canonical address, source codes copied from MMS application.
 */
package com.kl.test.ConversationTest;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.text.TextUtils;

public class RecipientIdCache {
	public static final Uri CONTENT_URI = Uri.parse("content://mms-sms/canonical-addresses");
	public static final String ID = "_id";
	public static final String ADDRESS = "address";
    private Map<Long, String> mCache;
    private Context mContext;
    private static RecipientIdCache mInstance;

    public static final class Entry {
        public long id;
        public String number;

        public Entry(long id, String number) {
            this.id = id;
            this.number = number;
        }
    };

    public static void init(Context context) {
    	mInstance = new RecipientIdCache(context);
        new Thread(new Runnable() {
            public void run() {
            	fill();
            }
        }).start();
    }

    RecipientIdCache(Context context) {
        mCache = new HashMap<Long, String>();
        mContext = context;
    }
    
    public static void fill() {
    	Context context = mInstance.mContext;
    	Log.d("Start to query canonicial address.");
    	Cursor cursor = context.getContentResolver().query(CONTENT_URI, null, null, null, null);
    	if(cursor.moveToFirst()) {
    		try {
    			synchronized(mInstance) {
    				mInstance.mCache.clear();
    				int idIndex = cursor.getColumnIndex(ID);
    				int addressIndex = cursor.getColumnIndex(ADDRESS);
    				do {
    					long id = cursor.getLong(idIndex);
    					String number = cursor.getString(addressIndex);
    					mInstance.mCache.put(id, number);
    				} while(cursor.moveToNext());
    			}
    		} finally {
    			cursor.close();
    		}
    	}
    	Log.d("Query canonicial address finished.");
    }
    
    public static List<Entry> getAddress(String spaceSepIds) {
    	synchronized(mInstance) {
            List<Entry> numbers = new ArrayList<Entry>();
            String[] ids = spaceSepIds.split(" ");
            for(String id : ids) {
                long longId;
                try {
                    longId = Long.parseLong(id);
                } catch (NumberFormatException ex) {
                    continue;
                }
                String number = mInstance.mCache.get(longId);	
                if(number == null) {
                	// the cache is still not ready, load it now.
                	Log.i("The canonicial cache is empty.");
                	fill();
                	number = mInstance.mCache.get(longId);
                }
                if(!TextUtils.isEmpty(number)) {
                	numbers.add(new Entry(longId, number));
                }
            }
            return numbers;
    	}
    }
}
