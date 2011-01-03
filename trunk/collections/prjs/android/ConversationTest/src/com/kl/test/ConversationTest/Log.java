package com.kl.test.ConversationTest;

public class Log {
	private static String mLogTag = "DefaultLog";
	
	public static void init(String tag) {
		mLogTag = tag;
	}
	
	public static void d(String str) {
		android.util.Log.d(mLogTag, str);
	}
	
	public static void i(String str) {
		android.util.Log.i(mLogTag, str);
	}
	
	public static void w(String str) {
		android.util.Log.w(mLogTag, str);
	}
	
	public static void e(String str) {
		android.util.Log.e(mLogTag, str);
	}
}