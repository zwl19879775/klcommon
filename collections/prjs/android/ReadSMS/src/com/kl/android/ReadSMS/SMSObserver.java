/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import android.content.Context;
import android.database.ContentObserver;
import android.os.Handler;

public class SMSObserver extends ContentObserver {
	private Context mContext;
	
	public SMSObserver( Context context, Handler h ) {
		super(h);
		mContext = context;
	}
	
	@Override
	public void onChange( boolean selfChange ) {
		super.onChange(selfChange);
		Log.d("SMSObserver:onChange");
		SMSReader.instance().read(mContext, -1);
	}
	
	public static void register(Context context) {
		Log.d("SMSObserver:register");
		SMSObserver ob = new SMSObserver( context, new Handler() );
		context.getContentResolver().registerContentObserver(Const.SMS_CONTENT,
				true, ob);
	}
}
