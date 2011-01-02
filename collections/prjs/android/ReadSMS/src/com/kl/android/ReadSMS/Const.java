/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import android.net.Uri;

public final class Const {
	public static final String LOGTAG = "ReadSMS";
	public static final String SDPATH = "/QuickSMS/";
	public static final Uri SMS_CONTENT = Uri.parse("content://sms");
	public static final Uri SMS_SENT_CONTENT = Uri.parse("content://sms/sent");
	public static final int MAX_MESSAGE_LEN = 160;
	public static final String EXTRA_THREADID = "threadID";
	public static final long NO_THREADID = -1;
	public static final int RECENT_CONTACT_CNT = 10;
}