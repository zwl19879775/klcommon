package com.kl.BlackList;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.telephony.SmsMessage;

public class SmsBroadcastReceiver extends BroadcastReceiver {
	public static final String SMS_RECEIVED_ACTION = "android.provider.Telephony.SMS_RECEIVED";
    @Override
    public void onReceive(Context ctx, Intent intent) {
    	Log.d("BroadcastReceiver called.");
    	if(intent.getAction().equals(SMS_RECEIVED_ACTION)) {
    		if(checkBlack(ctx, intent)) {
    			Log.d("Check black success, abort the broadcast.");
    			abortBroadcast();
    		}
    	}
    }    
    
    // TODO: if we received more than 1 SMS message at this time, if one of them is
    // blocked, and how to handle the others ?
    private boolean checkBlack(Context ctx, Intent intent) {
    	SmsMessage[] smsMsgs = BlockHelper.getSmsMessage(intent);
    	if(smsMsgs.length == 0) return false;
    	BlockHelper.Message msgs[] = BlockHelper.convertMessage(smsMsgs);
    	boolean ret = false;
    	for(BlockHelper.Message msg : msgs) {
    		BlackListCache.Item black = BlockHelper.getBlack(ctx, msg.address);
    		if(black != null) {
    			Log.i(String.format("Received black sms message from(%s), body(%s).", 
    					msg.address, msg.body));
    			ret = true;
    			BlockHelper.writeSmsBlockLog(ctx, black.id, msg.body);
    		}
    	}
    	return ret;
    }
}
