/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.Date;

public class SmsUtils {
	public static String getMessageDesc(ContactManager.Contact contact, SMSItem sms) {
		String desc = UIUtils.getString(AppStatus.appContext(), R.string.from_me);
		if(!sms.isSent()) {
			desc = contact.mName;
		}
		return desc + ":" + sms.mBody;
	}
	
    public static String getDateDesc( long time ) {
    	Date cal = new Date(time);
    	String desc = "";
    	if(isSameDay(cal)) {
    		int h = cal.getHours();
    		int m = cal.getMinutes();
    		desc += String.format("%02d:%02d", h, m);
    	}
    	else {
    		int month = cal.getMonth();
    		int day = cal.getDate();
    		desc += String.format("%02d", month);
    		desc += UIUtils.getString(AppStatus.appContext(), R.string.time_month);
    		desc += String.format("%02d", day);
    		desc += UIUtils.getString(AppStatus.appContext(), R.string.time_day);
    	}
    	return desc;
    }
    
    private static boolean isSameDay(Date d) {
    	int year = d.getYear();
    	int month = d.getMonth();
    	int day = d.getDay();
    	Date now = new Date();
    	if( year == now.getYear() &&
    		month == now.getMonth() &&
    		day == now.getDay() ) return true;
    	return false;
    }
}