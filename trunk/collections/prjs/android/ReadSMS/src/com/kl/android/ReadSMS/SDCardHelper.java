package com.kl.android.ReadSMS;

import java.io.File;
import java.io.FileOutputStream;
import android.os.Environment;

public class SDCardHelper {	
	
	public static boolean avaiable() {
    	String sdStatus = Environment.getExternalStorageState();
    	return sdStatus.equals(Environment.MEDIA_MOUNTED);
	}
	
	public static String getPath() {
		if(!avaiable()) return null;
		File root = Environment.getExternalStorageDirectory();
		return root.getAbsolutePath();
	}
	
	public static FileOutputStream openFile(String subPath, String fileName, boolean append) {
		if(!avaiable()) return null;
		String root = getPath();
    	FileOutputStream stream = null;
    	try {
    		String pathName = root + subPath;
    		File path = new File(pathName);
    		File file = new File(pathName + fileName);
    		if( !path.exists()) {
    			Log.d("Create the path:" + pathName);
    			path.mkdir();
    		}
    		if( !file.exists()) {
    			Log.d("Create the file:" + fileName);
    			file.createNewFile();
    		}
    		stream = new FileOutputStream(file, append);
    	} catch(Exception e) {
    		Log.e("Error on openFile on SDCard.");
    		e.printStackTrace();
    	}
    	return stream;
	}
}
