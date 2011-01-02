/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import android.content.Context;
import android.widget.Toast;

public class UIUtils {
	public static void toastInfo(Context context, String str) {
		Toast.makeText(context, str, Toast.LENGTH_SHORT).show();
	}
	
	public static String getString(Context context, int id) {
		return context.getResources().getString(id);
	}
}