package com.kl.android.Alarming;

import java.util.HashMap;

import android.app.Activity;
import android.content.SharedPreferences;

class WeekDesc {
	public static final String NOSET = "#ALL#";
	public static final int DAYCNT = 7;
	public boolean []mWeekday;
	
	// str is ListPreferenceMultiSelect saved string.
	public WeekDesc( String str ) {
		reset();
		if( !str.equals(NOSET) ) {
			String [] vals = ListPreferenceMultiSelect.parseStoredValue(str);
			for( int i = 0; i < vals.length; ++ i ) {
				mWeekday[str2Index(vals[i].trim())] = true;
			}
		}
	}
	public void reset() {
		mWeekday = new boolean [DAYCNT];
		for( int i = 0; i < DAYCNT; ++i ) {
			mWeekday[i] = false;
		}
	}
	public boolean isAllDisabled() {
		int c = 0;
		for( int i = 0; i < DAYCNT; ++i ) {
			if( mWeekday[i]) ++c;
		}
		return c == 0;
	}
	public String toDesc() {
		String s = "";
		int c = 0;
		for( int i = 0; i < DAYCNT; ++i ) {
			if( mWeekday[i] ) {
				s += getDesc(i);
				c++;
			}
		}
		if( c == DAYCNT ) return "每天";
		if( c == 0 ) return "未设置";
		return s;
	}
	private int str2Index( String s ) {
		// assume s is '001' '002' etc.
		return Integer.valueOf(s) - 1;
	}
	private String getDesc( int i ) {
		String desc[] = { "周一", "周二", "周三",
				"周四", "周五", "周六", "周日"};
		return desc[i];
	}
}

class ConfigData {
	public static final String LOGTAG = "KLAlarm";
	private static final String SEPSTR = "|";
	private static final String SEPSTR_REX = "\\|";
	private static final String TIME_ID = "time_id";
	private static final String RING_ID = "ring_id";
	private static final String WAKE_ID = "wake_id";
	private static final String ENABLE_ID = "enable_id";
	private static final String LABEL_ID = "label_id";
	private static final String REPEAT_ID = "repeat_multi_id";
	private static final String NO_RING = "__none__";
	public int mHour;
	public int mMinute;
	public String mRingID;
	public boolean mWakeFlag;
	public boolean mEnableFlag;
	public String mLabel;
	public String mRepeat;
	
	public ConfigData() {
		mHour = 12;
		mMinute = 0;
		mRingID = NO_RING;
		mWakeFlag = false;
		mEnableFlag = false;
		mLabel = "none";
		mRepeat = WeekDesc.NOSET;
	}
	public ConfigData( String str ) {
		String[] vals = str.split(SEPSTR_REX);
		if(vals!=null && vals.length > 6) {
			mHour = Integer.parseInt(vals[0].trim());
			mMinute = Integer.parseInt(vals[1].trim());
			mRingID = vals[2].trim();
			mWakeFlag = Boolean.parseBoolean(vals[3].trim());
			mEnableFlag = Boolean.parseBoolean(vals[4].trim());
			mLabel = vals[5].trim();
			mRepeat = vals[6].trim();	
		}
	}
	
	public ConfigData(SharedPreferences setting) {
		String time = setting.getString(TIME_ID, "00:00");
		String[] vals = time.split(":");
		mHour = Integer.valueOf(vals[0].trim());
		mMinute = Integer.valueOf(vals[1].trim());
		mRingID = setting.getString(RING_ID, NO_RING);
		if( mRingID.equals("")) {
			mRingID = NO_RING;
		}
		mWakeFlag = setting.getBoolean(WAKE_ID, false);
		mEnableFlag = setting.getBoolean(ENABLE_ID, false);
		mLabel = setting.getString(LABEL_ID, "none");
		mRepeat = setting.getString(REPEAT_ID, WeekDesc.NOSET);
	}
	public String encode() {
		String s = new String();
		s += String.valueOf(mHour);
		s += SEPSTR;
		s += String.valueOf(mMinute);
		s += SEPSTR;
		s += mRingID;
		s += SEPSTR;
		s += String.valueOf(mWakeFlag);
		s += SEPSTR;
		s += String.valueOf(mEnableFlag);
		s += SEPSTR;
		s += mLabel;
		s += SEPSTR;
		s += mRepeat;
		return s;
	}
	public boolean encode(SharedPreferences setting) {
		SharedPreferences.Editor editor = setting.edit();
		editor.putString(TIME_ID, Setting.formatTime(mHour, mMinute));
		if( mRingID.equals(NO_RING)) {
			editor.putString(RING_ID, "");
		}
		else {
			editor.putString(RING_ID, mRingID);
		}
		editor.putBoolean(WAKE_ID, mWakeFlag);
		editor.putBoolean(ENABLE_ID, mEnableFlag);
		editor.putString(LABEL_ID, mLabel);
		editor.putString(REPEAT_ID, mRepeat);
		editor.commit();
		return true;
	}
	
	static boolean isNoRing( String str ) {
		return str.equals(NO_RING);
	}
}

public class Config {
	private static final String ALARM_FILE = "alarm_all_cfg.xml";
	private int mCount;
	private int mOperIndex;
	private SharedPreferences mSetting;
	private static Config mInstance;
	
	public static Config instance() {
		if(mInstance == null) mInstance = new Config();
		return mInstance;
	}
	
	public boolean isInited() {
		return mSetting != null;
	}
	
	public int operIndex() {
		return mOperIndex;
	}
	
	public boolean init( Activity activity ) {
		mSetting = activity.getSharedPreferences(ALARM_FILE, Activity.MODE_PRIVATE);
		mCount = mSetting.getAll().size();
		return true;
	}
	
	// default shared preferences.
	public boolean begin(int index, SharedPreferences setting) {
		if(index >= mCount) return false;
		ConfigData cfg = retriveCfg(index);
		cfg.encode( setting );
		mOperIndex = index;
		return true;
	}	
	
	// default shared preferences.
	public boolean end(SharedPreferences setting) {
		if(mOperIndex < 0) return false;
		ConfigData cfg = new ConfigData(setting);
		String allStr = cfg.encode();
		SharedPreferences.Editor editor = mSetting.edit();
		editor.putString(String.valueOf(mOperIndex), allStr);
		editor.commit();
		mOperIndex = -1;
		return true;
	}
	
	public boolean fillItem(int index, HashMap<String, Object> item) {
		if(index >= mCount) return false;
		ConfigData cfg = retriveCfg(index);
		WeekDesc repeat = new WeekDesc(cfg.mRepeat);
		if( cfg.mEnableFlag ) item.put("enable_id", "ENABLE");
		else item.put("enable_id", "DISABLE" );
        item.put("ItemTitle", Setting.formatTime(cfg.mHour, cfg.mMinute));   
        item.put("ItemText", repeat.toDesc());  
        return true;
	}
	
	public ConfigData retriveCfg(int index) {
		String allStr = mSetting.getString( String.valueOf(index), "");
		ConfigData cfg = new ConfigData(allStr);
		return cfg;
	}
	
	public int addItem(SharedPreferences setting) {
		int index = mCount;
		++ mCount;
		// default value.
		ConfigData cfg = new ConfigData();
		cfg.encode(setting);
		String newItem = cfg.encode();
		SharedPreferences.Editor editor = mSetting.edit();
		editor.putString(String.valueOf(index), newItem);
		editor.commit();
		return index;
	}
	
	public boolean delItem() {
		if(mOperIndex == -1 || mCount == 0) return false;
		SharedPreferences.Editor editor = mSetting.edit();
		for( int i = mOperIndex; i < mCount - 1; ++i ) {
			String next = mSetting.getString(String.valueOf(i+1), "");
			editor.putString(String.valueOf(i), next);
		}
		editor.remove(String.valueOf(mCount-1));
		editor.commit();
		mOperIndex = -1;
		mCount--;
		return true;
	}
	
	public int Count() {
		return mCount;
	}
}
