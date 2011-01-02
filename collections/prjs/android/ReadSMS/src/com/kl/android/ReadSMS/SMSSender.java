/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.ArrayList;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.telephony.SmsManager;

class SentReceiver extends BroadcastReceiver {
	 @Override
	 public void onReceive(Context context, Intent intent) {
			switch (getResultCode()) {
			case Activity.RESULT_OK:
				UIUtils.toastInfo(context, UIUtils.getString(context, R.string.notify_send_success));
				break;
			case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
				UIUtils.toastInfo(context, "SMS generic failure actions");
				break;
			case SmsManager.RESULT_ERROR_RADIO_OFF:
				UIUtils.toastInfo(context, "SMS radio off failure actions");		
				break;
			case SmsManager.RESULT_ERROR_NULL_PDU:
				UIUtils.toastInfo(context, "SMS null PDU failure actions");
				break;
			}
	 }
}

class DeliverReceiver extends BroadcastReceiver {
	 @Override
	 public void onReceive(Context context, Intent intent) {
		 UIUtils.toastInfo(context, "SMS delivered actions");
	 }
}

public class SMSSender {
	public static final String SENT_SMS_ACTION = "SENT_SMS_ACTION";
	public static final String DELIVERED_SMS_ACTION = "DELIVERED_SMS_ACTION";
	
	public static void sendSMS(Context context, String phoneNum, String message) {
		SmsManager smsMgr = SmsManager.getDefault();
		Intent sentIntent = new Intent(SENT_SMS_ACTION);
		PendingIntent sentPI = PendingIntent.getBroadcast(context, 0, sentIntent, 0);
		Intent deliveredIntent = new Intent(DELIVERED_SMS_ACTION);
		PendingIntent deliveredPI = PendingIntent.getBroadcast(context, 0, deliveredIntent, 0);
		// sent receiver.
		context.registerReceiver(new SentReceiver(), new IntentFilter(SENT_SMS_ACTION));
		// delivered receiver.
		context.registerReceiver(new DeliverReceiver(), new IntentFilter(DELIVERED_SMS_ACTION));
		Log.d("Start to send a message to " + phoneNum);
		if( message.length() > Const.MAX_MESSAGE_LEN) {
			ArrayList<String> msgs = smsMgr.divideMessage(message);
			for (String msg : msgs) {
				smsMgr.sendTextMessage(phoneNum, null, msg, sentPI, deliveredPI);
				writeToSentDB(context, phoneNum, msg);
			}
		} 
		else {
			smsMgr.sendTextMessage(phoneNum, null, message, sentPI, deliveredPI);
			writeToSentDB(context, phoneNum, message);
		}
	}
	
	public static void sendSMS(Context context, String[] phoneNums, String message) {
		for(String num : phoneNums) {
			sendSMS(context, num, message);
		}
	}
	
	public static void writeToSentDB(Context context, String phoneNum, String body) {
		Log.d("write message to sent db.");
		ContentValues values = new ContentValues();
		values.put("address", phoneNum);
		values.put("body", body);
		context.getContentResolver().insert(Const.SMS_SENT_CONTENT, values);
	}
}
