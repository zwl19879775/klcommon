package com.kl.android.Alarming;

import android.app.Dialog;
import android.app.TimePickerDialog;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TimePicker;

public class Setting extends PreferenceActivity implements OnPreferenceClickListener,
	OnPreferenceChangeListener {
	public static final String OP_KEY = "op_type";
	public static final int OP_DEL = 1;
	public static final int OP_MODIFY = 2;
	private static final int MENU_DEL = Menu.FIRST;
	private static final String TIME_KEY = "time_id";
	private static final String LABEL_KEY = "label_id";
	private static final String REPEAT_KEY = "repeat_multi_id";
	private static final int TIME_DLG = 1;
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
		addPreferencesFromResource(R.xml.settings);
		
		Preference timePref = findPreference(TIME_KEY);
		timePref.setOnPreferenceClickListener(this);
		
		Preference labelPref = findPreference(LABEL_KEY);
		labelPref.setOnPreferenceChangeListener(this);

		Preference repeatPref = findPreference(REPEAT_KEY);
		repeatPref.setOnPreferenceChangeListener(this);
		
		setTimeView();
		setRepeatView();
		setLabelView();
	}
	public boolean onPreferenceClick(Preference preference) {   
		if( preference.getKey().equals(TIME_KEY)) {
			showDialog( TIME_DLG );
		}
        return true;   
    }  
    @Override  
    public boolean onPreferenceChange(Preference preference, Object newValue) {   
    	if( preference.getKey().equals(LABEL_KEY)) {
    		Preference labelPref = findPreference(LABEL_KEY);
    		labelPref.setSummary((CharSequence)newValue);
    	}
    	else if( preference.getKey().equals(REPEAT_KEY)) {
    		setRepeatView();
    	}
    	return true;   
    } 
	private void setTimeView() {
		SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this); 
		String timeStr = settings.getString(TIME_KEY, "00:00");
		Preference timePref = findPreference(TIME_KEY);
		timePref.setTitle(timeStr);
		timePref.setSummary("");
	}
	private void setRepeatView() {
		SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this); 
		String repeatStr = settings.getString(REPEAT_KEY, WeekDesc.NOSET);
		Preference repeatPref = findPreference(REPEAT_KEY);
		WeekDesc repeat = new WeekDesc(repeatStr);
		repeatPref.setSummary(repeat.toDesc());
	}
	private void setLabelView() {
		SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this); 
		String label = settings.getString(LABEL_KEY, "none");
		Preference labelPref = findPreference(LABEL_KEY);
		labelPref.setSummary(label);
	}
	public static String pad( int c ) {
		if( c > 10 ) {
			return String.valueOf(c);
		}
		else {
			return "0" + String.valueOf(c);
		}
	}
	public static String formatTime( int hour, int minute ) {
		return pad(hour) + ":" + pad(minute);
	}
	private void writeTime(int hour, int minute) {
		String timeStr = pad(hour) + ":" + pad(minute);
		SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this);   
		SharedPreferences.Editor editor = settings.edit();
		editor.putString( TIME_KEY, timeStr);
		editor.commit();
	}
    private TimePickerDialog.OnTimeSetListener mTimeSetListener = 
    	new TimePickerDialog.OnTimeSetListener() {
    		public void onTimeSet( TimePicker view, int hourOfday, int minute ) {
    			writeTime(hourOfday, minute);
    			setTimeView();
    		}
    };
    
    protected Dialog onCreateDialog( int id ) {
    	switch(id) {
    	case TIME_DLG:
    		SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this); 
    		String timeStr = settings.getString(TIME_KEY, "00:00");
    		String[] vals = timeStr.split(":");
    		int hour = Integer.valueOf(vals[0].trim());
    		int minute = Integer.valueOf(vals[1].trim());
    		return new TimePickerDialog(this, mTimeSetListener, hour, minute, false );
    	}
    	return null;
    }
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);
        menu.add(0, MENU_DEL, 0, R.string.menu_del);
        return true;
    }
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
        switch(item.getItemId()) {
            case MENU_DEL:
            {
            	delAlarm();
            }
            return true;
        }
        return super.onMenuItemSelected(featureId, item);
    }
    
    private void delAlarm() {
    	Bundle bundle = new Bundle();
    	bundle.putLong(OP_KEY, OP_DEL);
        Intent mIntent = new Intent();
        mIntent.putExtras(bundle);
        setResult(RESULT_OK, mIntent);
        finish();
    }
}
