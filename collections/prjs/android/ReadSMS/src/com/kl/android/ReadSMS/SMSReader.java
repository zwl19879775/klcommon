/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.ArrayList;

import android.content.Context;
import android.database.Cursor;

class SMSItem {
	public static final String ID = "_id";
	public static final String THREAD = "thread_id";
	public static final String ADDRESS = "address";
	public static final String PERSON = "person";
	public static final String DATE = "date";
	public static final String READ = "read";
	public static final String BODY = "body";
	public static final String SUBJECT = "subject";
	public static final String TYPE = "type";
	
	public String mAddress;
	public String mBody;
	public String mSubject;
	public long mID;
	public long mThreadID;
	public long mDate;
	public long mRead;
	public long mPerson;
	public long mType;
	
	private static int mIdIdx;
	private static int mThreadIdx;
	private static int mAddrIdx;
	private static int mPersonIdx;
	private static int mDateIdx;
	private static int mReadIdx;
	private static int mBodyIdx;
	private static int mSubjectIdx;
	private static int mTypeIdx;
	
	public SMSItem(Cursor cur) {
		mID = cur.getLong(mIdIdx);
		mThreadID = cur.getLong(mThreadIdx);
		mAddress = cur.getString(mAddrIdx);
		mPerson = cur.getLong(mPersonIdx);
		mDate = cur.getLong(mDateIdx);
		mRead = cur.getLong(mReadIdx);
		mBody = cur.getString(mBodyIdx);
		mSubject = cur.getString(mSubjectIdx);
		mType = cur.getLong(mTypeIdx);
	}
	
	public static void initIdx(Cursor cur) {
		mIdIdx = cur.getColumnIndex( ID );
		mThreadIdx = cur.getColumnIndex( THREAD );
		mAddrIdx = cur.getColumnIndex( ADDRESS );
		mPersonIdx = cur.getColumnIndex( PERSON );
		mDateIdx = cur.getColumnIndex( DATE );
		mReadIdx = cur.getColumnIndex( READ );
		mBodyIdx = cur.getColumnIndex( BODY );
		mSubjectIdx = cur.getColumnIndex( SUBJECT );
		mTypeIdx = cur.getColumnIndex( TYPE );
	}
	
	public String toString() {
		String ret = ID + ":" + String.valueOf(mID) + " " +
			THREAD + ":" + String.valueOf(mThreadID) + " " +   
			ADDRESS + ":" + mAddress + " " + 
			PERSON + ":" + String.valueOf(mPerson) + " " + 
			DATE + ":" + String.valueOf(mDate) + " " +
			READ + ":" + String.valueOf(mRead) + " " + 
			TYPE + ":" + String.valueOf(mType) + " " +
			SUBJECT + ":" + mSubject + " " + 
			BODY + ":" + mBody; 
		return ret;
	}
	
	public boolean isSent() {
		final long SENT = 2;
		return mType == SENT;
	}
}

public class SMSReader {
	private ArrayList<SMSItem> mSmsList = new ArrayList<SMSItem>();
	private static SMSReader mInst = new SMSReader();
	/// Observers will be notified after SMSReader read a new SMS message.
	private ArrayList<SMSObserver> mObservers = new ArrayList<SMSObserver>();
	private long mRecentDate = 0;
	
	public interface SMSStore {
		void onAdd(Context context, SMSItem sms);
	}
	
	public interface SMSObserver {
		void onNewItem(Context context, SMSItem sms);
	}

	public SMSReader() {
		
	}
	
	public static SMSReader instance() {
		return mInst;
	}
	
	SMSItem get(int idx) {
		return mSmsList.get(idx);
	}
	
	int count() {
		return mSmsList.size();
	}

	///
	/// @param date if equal 0, read all SMS, if < 0, read the new SMS not
	/// in the list already.
	int read(Context context, long date) {
		int size = mSmsList.size();
		if( date < 0 ) date = mRecentDate;
		Log.d(String.format("Start to read SMS date > %d, size=%d",
				date, size));
		Cursor cur = context.getContentResolver().query(Const.SMS_CONTENT,
				null, " date>?", new String [] { String.valueOf( date ) }, "date desc");
		if( cur != null && 
			cur.moveToFirst()) {
			SMSItem.initIdx(cur);
			do {
				SMSItem item = new SMSItem(cur);
				mSmsList.add(item);
				if(item.mDate > mRecentDate) mRecentDate = item.mDate;
			} while(cur.moveToNext());
			cur.close();
		}
		Log.d(String.format("Success read %d messages.", count() - size ));
		for( int i = size; i < count(); ++i ) {
			notifyObservers(context, mSmsList.get(i));
		}
		return count();
	}

	void clear() {
		mSmsList.clear();
		mObservers.clear();
	}
	
	void traverse(Context context, SMSStore s) {
		for(int i = 0; i < count(); ++i ) {
			s.onAdd(context, mSmsList.get(i));
		}
	}
	
	public void registerObserver(SMSObserver ob) {
		mObservers.add(ob);
		Log.d(String.format("registerObserver size=%d", mObservers.size()));
	}
	
	public void cancelObserver(SMSObserver ob) {
		mObservers.remove(ob);
		Log.d(String.format("cancelObserver size=%d", mObservers.size()));
	}
	
	private void notifyObservers(Context context, SMSItem sms) {
		for(SMSObserver ob : mObservers) {
			ob.onNewItem(context, sms);
		}
	}
}
