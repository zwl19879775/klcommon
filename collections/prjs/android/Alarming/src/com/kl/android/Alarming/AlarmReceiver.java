package com.kl.android.Alarming;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

public class AlarmReceiver extends BroadcastReceiver
{
    @Override
    public void onReceive(Context context, Intent intent)
    {
        Bundle extras = intent.getExtras();
        Long index = null;
        if( extras != null )
        {
        	index = extras.getLong("Index");
        	long lindex = index.longValue();
        	Log.d(ConfigData.LOGTAG, "Intent value " + lindex);
        }
        else
        {
        	Log.d(ConfigData.LOGTAG, "NULL Intent value.");
        }
    	Intent i = new Intent(context, AlarmAlert.class); 
    	Bundle bundleRet = new Bundle(); 
    	bundleRet.putString("STR_CALLER", ""); 
    	if( index != null )
    	{
    		bundleRet.putLong("Index", index.longValue());
    	}
    	i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK); 
    	i.putExtras(bundleRet); 
    	context.startActivity(i);
    }
}
