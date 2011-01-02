/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

import android.content.Context;

/// Store all the SMS messages by thread id(session id).
public class ThreadSmsStore implements SMSReader.SMSStore, 
	SMSReader.SMSObserver {
	/// Chat session implementation.
	public class ThreadItem {
		public long mThreadID;
		public List<SMSItem> mSmsList = new ArrayList<SMSItem>();
		public ContactManager.Contact mContact;
		
		public long recentDate() {
			if(mSmsList.size() > 0) {
				return mSmsList.get(0).mDate;
			}
			return 0;
		}
		
	}
	
	/// Used to sort the whole thread messages.
	private class ItemComparator implements Comparator<ThreadItem> {
		public int compare(ThreadItem left, ThreadItem right) {
			long ldate = left.recentDate();
			long rdate = right.recentDate();
			if( ldate == rdate ) return 0;
			if( ldate < rdate ) return -1;
			return 1;
		}
	}
	
	/// Traverser.
	public interface Traverser {
		void onTraverse( ThreadItem item);
	}
	
	/// Usually UI cares about this.
	public interface OnChanged {
		void onChanged();
	}
	
	private static ThreadSmsStore mInst = new ThreadSmsStore();
	private OnChanged mObserver;
	
	// <ThreadID, thread>, thread: <contact, smsList>
	private HashMap<String, ThreadItem> mAllSms = new HashMap<String, ThreadItem>();

	/// Must called after the first initialization.
	public void observe() {
		SMSReader.instance().registerObserver(this);	
	}

	public static ThreadSmsStore instance() {
		return mInst;
	}

	public void setObserver(OnChanged ob) {
		mObserver = ob;
	}
	
	public void onAdd(Context context, SMSItem sms) {
		if(mAllSms.containsKey(toKey(sms))) {
			ThreadItem item = mAllSms.get(toKey(sms));
			item.mSmsList.add(sms);
		}
		else {
			addNew(context, sms);
		}
	}
	
	private void addNew(Context context, SMSItem sms) {
		ThreadItem item = new ThreadItem();
		item.mThreadID = sms.mThreadID;
		item.mContact = ContactManager.instance().queryContactByAddr(context, sms.mAddress);
		if( item.mContact == null ) item.mContact = ContactManager.instance().getTempContact(sms.mAddress);
		item.mSmsList.add(sms);
		mAllSms.put(toKey(sms), item);	
	}
	
	public void onNewItem(Context context, SMSItem sms) {
		if(mAllSms.containsKey(toKey(sms))) {
			// suppose the new SMS is newer than the exist SMSs.
			ThreadItem item = mAllSms.get(toKey(sms));
			item.mSmsList.add(0, sms);
		}
		else {
			addNew(context, sms);
		}
		if(mObserver != null) mObserver.onChanged();
	}
	
	public void tranverse( Traverser t ) {
		Collection<ThreadItem> vals = mAllSms.values();
		ArrayList<ThreadItem> items = new ArrayList<ThreadItem>( vals );
		Collections.sort(items, new ItemComparator());
		int size = items.size();
		for( int i = 0; i < size; ++ i ) {
			t.onTraverse(items.get(size-i-1));
		}
	}
	
	public ThreadItem get(long threadID) {
		return mAllSms.get(String.valueOf(threadID));
	}
	
	private String toKey(final SMSItem sms) {
		return String.valueOf(sms.mThreadID);
	}
	
	public void clear() {
		mAllSms.clear();
	}
}
