/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import android.content.Context;

/// Global application status to manage global logic data.
/// When the application(process) started as new, this class
/// will be loaded as new, so mStartNew will be true.
public class AppStatus {
	private static boolean mStartNew = true;
	private static Context mContext;
	
	public static Context appContext() {
		return mContext;
	}
	
	public static boolean checkInit(Context context) {
		mContext = context;
		if(mStartNew) {
			// the process start as new.
			mStartNew = false;
			initialize(context);
			return true;
		}
		// else the process start from previous status, so we do not
		// need to initialize.
		return false;
	}
	
	private static void initialize(Context context) {
		// register content observer on SMS database.
        SMSObserver.register(context);
    	SMSReader smsReader = SMSReader.instance();
    	ThreadSmsStore threadStore = ThreadSmsStore.instance();
    	// read all the SMS in database.
    	Log.logTimeStart("ReadAllSMS");
    	smsReader.read(context, 0);
    	Log.logTimeEnd("ReadAllSMS");
    	// build thread reference to SMS.
    	Log.logTimeStart("MakeSmsConversation");
    	smsReader.traverse(context, threadStore);
    	Log.logTimeEnd("MakeSmsConversation");
    	// observe on new SMS.
    	threadStore.observe();
    	// register exporter.
    	TxtExporter.register();
    	// observe on contact list.
    	ContactObserver.register(context);
	}
}
