package com.kl.android.ReadSMS;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Stack;

public class Log {
	private final static class TimeStamp {
		long mBeginTime;
		String mTag;
	}
	private static final String SDLOG = "sdlog.txt";
	private static Stack<TimeStamp> mTimeStamps = new Stack<TimeStamp>();
	
	public static void i(String s) {
		android.util.Log.i(Const.LOGTAG, s);
	}
	
	public static void d(String s) {
		android.util.Log.d(Const.LOGTAG, s);
	}
	
	public static void e(String s) {
		android.util.Log.e(Const.LOGTAG, s);
	}
	
	public static void w(String s) {
		android.util.Log.w(Const.LOGTAG, s);
	}
	
	public static void logTimeStart(String tag) {
		TimeStamp s = new TimeStamp();
		s.mBeginTime = System.currentTimeMillis();
		s.mTag = tag;
		mTimeStamps.push(s);
		writeToSD(tag + " begins.");
	}
	
	public static void logTimeEnd(String tag) {
		TimeStamp s = mTimeStamps.peek();
		while(!s.mTag.equals(tag) && 
			!mTimeStamps.empty()) 
			s = mTimeStamps.pop(); 
		if(!mTimeStamps.empty()) {
			String info = s.mTag + String.format(" use %d mills.", System.currentTimeMillis() - s.mBeginTime);
			writeToSD(info);
		}
	}
	
	private static void writeToSD(String s) {
			FileOutputStream stream = SDCardHelper.openFile(Const.SDPATH, SDLOG, true);
			if(stream == null) return;
			try {
				s += "\r\n";
				stream.write(s.getBytes());
				stream.close();
			}catch(IOException e) {
				w("Write time log failed.");
			}
	}
}
