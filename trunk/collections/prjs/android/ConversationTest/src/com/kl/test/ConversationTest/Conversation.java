package com.kl.test.ConversationTest;

import java.util.Date;
import java.util.List;

import android.content.AsyncQueryHandler;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;

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
    /// data.
    public long mDate;
    public long mMessageCnt;
    public String mSnippet;
    public String mRecipientIds;
    public long mID;
    public ContactManager.Contact mContact;
    
    public static Conversation from(Context context, Cursor cur) {
    	long t = System.currentTimeMillis();
    	Conversation conv = new Conversation();
    	int date = cur.getColumnIndex(DATE);
    	int msgCnt = cur.getColumnIndex(MESSAGE_COUNT);
    	int snippet = cur.getColumnIndex(SNIPPET);
    	int id = cur.getColumnIndex(ID);
    	conv.mID = cur.getLong(id);
    	conv.mDate = cur.getLong(date);
    	conv.mMessageCnt = cur.getLong(msgCnt);
    	conv.mSnippet = cur.getString(snippet);
    	conv.mRecipientIds = cur.getString(cur.getColumnIndex(RECIPIENT_IDS));
    	conv.mContact = ContactManager.instance().asyncQueryContact(context, conv.getNumber()) ;
    	Log.d(String.format("Conversation time:%d", System.currentTimeMillis() - t));
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
