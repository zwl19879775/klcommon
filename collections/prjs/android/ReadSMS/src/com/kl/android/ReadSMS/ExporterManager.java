/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.HashMap;

import com.kl.android.ReadSMS.ThreadSmsStore.ThreadItem;


interface SmsExporter {
	boolean export(ThreadSmsStore.ThreadItem threadItem);
}

public class ExporterManager {
	private static ExporterManager mInst = new ExporterManager();
	private HashMap<String, SmsExporter> mExporters = new HashMap<String, SmsExporter>();

	public static ExporterManager instance() {
		return mInst;
	}
	
	public void register(String type, SmsExporter exp) {
		mExporters.put(type, exp); 
	}
	
	public SmsExporter get(String type) {
		return mExporters.get(type);
	}
	
	public boolean export(String type, long threadID) {
		SmsExporter exp = get(type);
		if(exp == null) {
			Log.w("Exporter not found:" + type);
			return false;
		}
		ThreadSmsStore.ThreadItem item = ThreadSmsStore.instance().get(threadID);
		if(item == null) {
			Log.w(String.format("Sms thread %d not found.", threadID ) );
			return false;
		}
		return exp.export(item);
	}
	
	public boolean exportAll(String type) {
		SmsExporter exp = get(type);
		if(exp == null) {
			Log.w("Exporter not found:" + type);
			return false;
		}
		class Traverser implements ThreadSmsStore.Traverser {
			private SmsExporter mExporter = null;
			Traverser(SmsExporter exp) {
				mExporter = exp;
			}
			public void onTraverse(ThreadItem item) {
				if( !mExporter.export(item) ) {
					Log.w("Export sms failed:" + item.mContact.mName );
				}
			}
		}
		ThreadSmsStore.instance().tranverse( new Traverser(exp) );
		return true;
	}
}
