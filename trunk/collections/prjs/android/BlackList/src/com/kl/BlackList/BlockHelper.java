package com.kl.BlackList;

import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.telephony.SmsMessage;
import com.kl.android.BlackListProvider.BlackList;

public class BlockHelper {
	public static final class Message {
		String address;
		String body;
	}
	
	public static BlackListCache.Item getBlack(Context context, String address) {
		Cursor cursor = context.getContentResolver().query(BlackList.CONTENT_URI, 
				BlackListCache.PROJECTION, BlackList.ADDRESS + "=?", 
				new String[] { address }, null);
		if(!cursor.moveToFirst()) {
			Log.d("Not found the black item:" + address);
			return null;
		}
		BlackListCache.Item item = new BlackListCache.Item();
		item.address = address;
		item.date = cursor.getLong(BlackListCache.DATE_IDX);
		item.id = cursor.getLong(BlackListCache.ID_IDX);
		return item;
	}
	
	public static SmsMessage[] getSmsMessage(Intent intent) {
		Bundle bundle = intent.getExtras();
		if(bundle == null) {
			Log.w("getSmsMessage get a null bundle");
			return null;
		}
		Object[] pdus = (Object[])bundle.get("pdus");   
        SmsMessage[] smg = new SmsMessage[pdus.length];   
        for(int i=0; i<pdus.length;i++){   
            smg[i]=SmsMessage.createFromPdu((byte[])pdus[i]);   
        }   
        Log.d(String.format("Receive %d sms messages.", smg.length));
        return smg;
	}
	
	public static Message[] convertMessage(SmsMessage[] smsMsgs) {
		Message[] msg = new Message[smsMsgs.length];
		for(int i = 0; i < smsMsgs.length; ++i) {
			msg[i] = new Message();
			msg[i].address = smsMsgs[i].getDisplayOriginatingAddress();
			msg[i].body = smsMsgs[i].getDisplayMessageBody();
			Log.d(String.format("convert msg:(%s)(%s).", msg[i].address,
					msg[i].body));
		}
		return msg;
	}
	
	public static boolean writeSmsBlockLog(Context context, long blackID, String body) {
		ContentValues val = new ContentValues();
		val.put(BlackList.BlockLog.BLACK_ID, blackID);
		val.put(BlackList.BlockLog.DESC, body);
		val.put(BlackList.BlockLog.TYPE, BlackList.BlockLog.TYPE_MMS);
		context.getContentResolver().insert(BlackList.BlockLog.CONTENT_URI, val);
		return true;
	}
}
