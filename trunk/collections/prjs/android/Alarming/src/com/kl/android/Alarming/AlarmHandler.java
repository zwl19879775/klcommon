package com.kl.android.Alarming;

import java.util.Calendar;

import android.app.Activity;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.Toast;

public class AlarmHandler {
	private static AlarmHandler mInstance;
	
	public static AlarmHandler instance() {
		if( mInstance == null ) mInstance = new AlarmHandler();
		return mInstance;
	}
	
	public boolean schedule(ConfigData cfg, Context context, int index, boolean debug) {
		debug = false;
		if( !cfg.mEnableFlag ) return false;
		WeekDesc week = new WeekDesc(cfg.mRepeat);
		if( week.isAllDisabled()) return false;
		long mills = getAlarmTime(week, cfg);
		if( mills <= 0 ) {
			Toast.makeText(context, "Warning: mills <= 0.", Toast.LENGTH_LONG).show();
			return false;
		}
    	Intent intent = new Intent(context, AlarmReceiver.class);
    	addExtra(intent, index);
    	// the 'index' parameter here is an identifier for every alarm.
        PendingIntent sender = PendingIntent.getBroadcast(context,
                index, intent, 0);
 
        if( debug ) mills = 60 * 1000;
        AlarmManager am = (AlarmManager)context.getSystemService(Context.ALARM_SERVICE);
        am.set(AlarmManager.RTC_WAKEUP, getNowInMills() + mills, sender);
        notifyTime(context, cfg.mLabel, mills);
        return true;
	}	

	public void cancel(Context context, int index) {
		Intent intent = new Intent(context, AlarmReceiver.class);
		PendingIntent sender = PendingIntent.getBroadcast(context, index, intent, 0);
		AlarmManager am = (AlarmManager)context.getSystemService(Context.ALARM_SERVICE);
		am.cancel(sender);
	}
	
	public ConfigData retrieveCfg(int index, Activity activity) {
		if( !Config.instance().isInited() ) {
			Config.instance().init(activity);
		}
		return Config.instance().retriveCfg(index);
	}
	
	private void notifyTime( Context context, String label, long mills ) {
		long minutes = mills / 1000 / 60;
		String desc = "";
		if( minutes < 60 ) {
			desc = String.valueOf(minutes) + " minutes";
		}
		else {
			desc = String.valueOf(minutes/60) + " hours";
		}
		Toast.makeText(context, "The alarm [" + label + "] will alert after " + desc, 
			Toast.LENGTH_LONG).show();
	}
	
	private long getAlarmTime(WeekDesc week, ConfigData cfg) {
		long ret = 0;
		Calendar calendar = Calendar.getInstance();
		int dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK);
		// dayOfWeek starts at 2.
		dayOfWeek = convertDayOfWeek(dayOfWeek);
		int hour = calendar.get(Calendar.HOUR_OF_DAY);
		int minute = calendar.get(Calendar.MINUTE);
		boolean exceptToday = !canBeToday(cfg, hour, minute);
		int dayCnt = getDayCnt(week, dayOfWeek, exceptToday);
		ret = getMills(dayCnt, cfg, hour, minute);
		
		Log.i(ConfigData.LOGTAG, "schedule " + dayCnt + " days, " + ret + " mills.");
		return ret;
	}
	
	private int getDayCnt(WeekDesc week, int dayOfWeek, boolean exceptToday) {
		int dayCnt = 0;
		if( exceptToday ){
			++ dayOfWeek;
			++ dayCnt;
		}
		if( dayOfWeek >= WeekDesc.DAYCNT ) dayOfWeek = 0;
		for( int i = dayOfWeek; i < WeekDesc.DAYCNT; ++i, ++dayCnt ) {
			if( week.mWeekday[i] ) return dayCnt;
		}
		if( dayCnt > 0 ) return dayCnt;
		for( int i = 0; i < dayOfWeek; ++i, ++dayCnt ) {
			if( week.mWeekday[i] ) break;
		}
 		return dayCnt;
	}
	
	private long getMills(int dayCnt, ConfigData cfg, int nowHour, int nowMinute) {
		long mills = 0;
		if( dayCnt == 0 ) {// triggered today 
			mills = ( cfg.mHour - nowHour ) * 60 * 60 * 1000;
			mills += ( cfg.mMinute - nowMinute ) * 60 * 1000;
		}
		else {
			int sumHour = 24 - nowHour + cfg.mHour;
			mills = sumHour * 60 * 60 * 1000;
			mills += ( cfg.mMinute - nowMinute ) * 60 * 1000;
			dayCnt --;
			mills += dayCnt * 24 * 60 * 60 * 1000;
		}
		return mills;
	}
	
	private boolean canBeToday( ConfigData cfg, int nowHour, int nowMinute ) {
		if( nowHour < cfg.mHour ) return true;
		if( nowHour == cfg.mHour && 
			nowMinute < cfg.mMinute ) return true;
		return false;
	}
	
	private int convertDayOfWeek( int dayOfWeek ) {
		if( dayOfWeek >= 2 ) return dayOfWeek - 2;
		return dayOfWeek + 5;
	}
	
	private long getNowInMills() {
		Calendar calendar = Calendar.getInstance();
		return calendar.getTimeInMillis();
	}
	
	private void addExtra( Intent intent, int index ) {
		Bundle bundle = new Bundle();
    	bundle.putLong("Index", index);
    	intent.putExtras( bundle );
	}
}
