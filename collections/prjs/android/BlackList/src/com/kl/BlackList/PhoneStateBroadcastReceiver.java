package com.kl.BlackList;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.RemoteException;
import android.telephony.TelephonyManager;

import com.android.internal.telephony.ITelephony;

public class PhoneStateBroadcastReceiver extends BroadcastReceiver {
	private static final int CALL_STATE = 1;
	private static final int HANGUP_STATE = 0;
	@Override
	public void onReceive(Context context, Intent intent) {
	    ITelephony iTelephony = getTelephone(context);
	    if(!isCall(iTelephony)) {
	    	return;
	    }
	    Bundle bundle = intent.getExtras();
	    String address = bundle.getString("incoming_number");
	    Log.i("Phone got a call from " + address);
	    BlackListCache.Item black = BlockHelper.getBlack(context, address);
	    if(black == null) {
	    	Log.d(String.format("The phone %s is not in black list.", address));
	    	return;
	    }
	    Log.i(String.format("The phone %s is in the black list, block it.", address));
	    // the address is in the black list, block it.
	    try {
	    	iTelephony.endCall();
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		// write log.
		BlockHelper.writePhoneCallBlockLog(context, black.id, address);
	}
	
	private ITelephony getTelephone(Context context) {
	    TelephonyManager tManager = (TelephonyManager)context.getSystemService(Context.TELEPHONY_SERVICE);
	    Class <TelephonyManager> c = TelephonyManager.class;
	    Method getITelephonyMethod = null;
	    try {
	    	getITelephonyMethod = c.getDeclaredMethod("getITelephony", (Class[])null);
	    	getITelephonyMethod.setAccessible(true);
	    } catch (SecurityException e) {
	    	e.printStackTrace();
	    	Log.e("Get ITelephony method failed.");
	    } catch (NoSuchMethodException e) {
	    	e.printStackTrace();
	    	Log.e("Get ITelephony method failed.");
	    }
	    
	    ITelephony iTelephony = null;
	    try {
	    	iTelephony = (ITelephony) getITelephonyMethod.invoke(tManager, (Object[])null);
	    } catch (IllegalArgumentException e) {
	    	e.printStackTrace();
	    } catch (IllegalAccessException e) {
	    	e.printStackTrace();
	    } catch (InvocationTargetException e) {
	    	e.printStackTrace();
	    }	
	    return iTelephony;
	}
	
	private boolean isCall(ITelephony telephony) {
		int s = HANGUP_STATE;
		try {
			s = telephony.getCallState();
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		return s == CALL_STATE;
	}
}
