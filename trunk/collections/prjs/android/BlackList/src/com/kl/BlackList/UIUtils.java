/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.BlackList;

import java.text.DateFormat;
import java.util.Date;

import android.content.Context;
import android.widget.Toast;

public class UIUtils {
	public static void toastInfo(Context context, String str) {
		Toast.makeText(context, str, Toast.LENGTH_SHORT).show();
	}
	
	public static String getString(Context context, int id) {
		return context.getResources().getString(id);
	}
	
	public static String getDateFormat(long date) {
		return DateFormat.getInstance().format(new Date(date));
	}
}