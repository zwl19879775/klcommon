package com.kl.test.ConversationTest;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.content.AsyncQueryHandler;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;

class ConversationCache {
	private static Map<Long, Conversation> sCache = new HashMap<Long, Conversation>();
	
	public static Conversation get(long id) {
		return sCache.get(id);
	}
	
	public static void add(Conversation conv) {
		sCache.put(conv.mID, conv);
	}
}

public class Conversation {
	public static final Uri FULL_CONTENT_URI = Uri.parse("content://mms-sms/conversations");
	public static final Uri CONTENT_URI = FULL_CONTENT_URI .buildUpon().
		appendQueryParameter("simple", "true").build();
    public static final String ID = "_id";
    public static final String DATE = "date";
    public static final String MESSAGE_COUNT = "message_count";
    public static final String RECIPIENT_IDS = "recipient_ids";
    public static final String SNIPPET = "snippet";
    public static final String SNIPPET_CS = "snippet_cs";
    public static final String READ = "read";
    public static final String ERROR = "error";
    public static final String HAS_ATTACHMENT= "has_attachment";
    /// When used with CursorAdapter, must query "_id" column.
    private static final String[] PROJECTION = { 
    	ID, DATE, MESSAGE_COUNT, SNIPPET, RECIPIENT_IDS };
    private static final int ID_INDEX = 0;
    private static final int DATE_INDEX = 1;
    private static final int MESSAGE_COUNT_INDEX = 2;
    private static final int SNIPPET_INDEX = 3;
    private static final int RECIPIENT_IDS_INDEX = 4;
    /// data.
    public long mDate;
    public long mMessageCnt;
    public String mSnippet;
    public String mRecipientIds;
    public long mID;
    public ContactManager.Contact mContact;
    
    /// these Log here will cost much run time too.
    /// If there waste much time in this function, it will make
    /// the list view seems not smooth.
    public static Conversation from(Context context, Cursor cur) {
    	//long t = System.currentTimeMillis();
    	long id = cur.getLong(ID_INDEX);
    	Conversation conv = ConversationCache.get(id);
    	if(conv == null) {
    		conv = new Conversation();
    		conv.mID = id;
    		conv.mDate = cur.getLong(DATE_INDEX);
    		conv.mMessageCnt = cur.getLong(MESSAGE_COUNT_INDEX);
    		conv.mSnippet = cur.getString(SNIPPET_INDEX);
    		conv.mRecipientIds = cur.getString(RECIPIENT_IDS_INDEX);
    		conv.mContact = ContactManager.instance().asyncQueryContact(context, conv.getNumber()) ;
    		ConversationCache.add(conv);
    	}
    	else {
    		//Log.d(String.format("Get in cache %d", id));
    	}
    	//Log.d(String.format("Conversation time:%d", System.currentTimeMillis() - t));
    	return conv;
    }
    
    public static Cursor queryAll(Context context) {
    	Cursor cur = context.getContentResolver().query(CONTENT_URI, PROJECTION,
    			null, null, null);
    	return cur;
    }
    
    public static void asyncQueryAll(AsyncQueryHandler handler, int token) {
    	handler.cancelOperation(token);
    	handler.startQuery(token, null, CONTENT_URI, PROJECTION, null, null, null);
    }
    
    public String getDateDesc() {
    	Date cal = new Date(mDate);
    	int month = cal.getMonth();
    	int day = cal.getDate();
    	return String.format("%02d.%02d", month, day);
    }
    
    public String getCount() {
    	return String.valueOf(mMessageCnt);
    }
    
    public String getNumber() {
    	List<RecipientIdCache.Entry> numbers = RecipientIdCache.getAddress(mRecipientIds);
    	String number = "no number";
    	if(numbers.size() > 0) number = numbers.get(0).number;    	
    	return number;
    }
}
