/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.io.FileOutputStream;
import java.io.IOException;

public class TxtExporter implements SmsExporter {
	public static final String EXPORT_TYPE = "TextExporter";
	// '\r' is for these Windows user.
	private static final String NEWLINE = "\r\n";
	
	public boolean export(ThreadSmsStore.ThreadItem item) {
		FileOutputStream stream = SDCardHelper.openFile(Const.SDPATH, getFileName(item), false);
		if(stream == null) {
			Log.e("Txt.export failed.");
			return false;
		}
		int size = item.mSmsList.size();
		for( int i = size - 1; i >= 0; --i ) {
			SMSItem sms = item.mSmsList.get(i);
			String desc = SmsUtils.getMessageDesc(item.mContact, sms);
			desc += NEWLINE;
			String time = SmsUtils.getDateDesc(sms.mDate);
			time += ( NEWLINE + NEWLINE );
			try {
				stream.write(desc.getBytes());
				stream.write(time.getBytes());
			}
			catch(IOException e) {
				Log.w("Write some text failed.");
			}
		}
		try {
			stream.close();
		}
		catch(IOException e) {
			Log.w("Close stream failed.");
		}
		Log.i(String.format("Export thread %d to txt success.", item.mThreadID));
		return true;
	}
	
	public static void register() {
		ExporterManager.instance().register(EXPORT_TYPE, new TxtExporter());
	}
	
	private String getFileName(ThreadSmsStore.ThreadItem item) {
		return "chat_" + item.mContact.mName + ".txt";
	}
}
