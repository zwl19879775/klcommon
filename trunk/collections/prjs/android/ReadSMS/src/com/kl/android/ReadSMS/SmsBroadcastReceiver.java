/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class SmsBroadcastReceiver extends BroadcastReceiver {
	public static final String SMS_RECEIVED_ACTION = "android.provider.Telephony.SMS_RECEIVED";
	private static final int SMS_NOTIFY_ID = 1;
    @Override
    public void onReceive(Context ctx, Intent intent) {
    	if(intent.getAction().equals(SMS_RECEIVED_ACTION)) {
    		String ns = Context.NOTIFICATION_SERVICE;
    		NotificationManager notifyMgr = (NotificationManager) ctx.getSystemService(ns);
    		int icon = R.drawable.icon;
    		CharSequence tickerText = "You got new message";
    		long when = System.currentTimeMillis();
    		Notification notification = new Notification(icon, tickerText, when);
    		
    		CharSequence contentTitle = "New Message";
    		CharSequence contentText = "Click here to view the full message.";
    		Intent notificationIntent = new Intent(ctx, ReadSMS.class);
    		PendingIntent contentIntent = PendingIntent.getActivity(ctx, 0, notificationIntent, 0);
    		notification.setLatestEventInfo(ctx, contentTitle, contentText, contentIntent);
    		notification.flags |= Notification.FLAG_AUTO_CANCEL;
    		
    		notifyMgr.notify(SMS_NOTIFY_ID, notification);
    	}
    }    
}
