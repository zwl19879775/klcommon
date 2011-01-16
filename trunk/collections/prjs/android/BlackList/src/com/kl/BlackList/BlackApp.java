package com.kl.BlackList;

import android.app.Application;
import android.content.Context;

public class BlackApp extends Application {
	private static BlackApp mApp;
	private boolean mInitedAddPage = false;
	
	@Override
	public void onCreate() {
		super.onCreate();
		mApp = this;
		init();
	}
	
	public static BlackApp app() {
		return mApp;
	}
	
	private void init() {
		Log.init("BlackList");
		Log.i("Application start init...");
		Context context = getApplicationContext();
		BlackListCache.init(context);
		Log.i("Application init completed.");
	}
	
	public void initAddPageData() {
		Log.i("Init add page data...");
		Context context = getApplicationContext();
		Conversation.init(context);
		RecipientIdCache.init(context);
		Contact.init(context);
		CallLog.init(context);	
		Log.i("Init add page data completed.");
		mInitedAddPage = true;
	}
	
	public void checkInitBlockLog() {
		BlockLog inst = BlockLog.inst();
		if( inst == null) {
			BlockLog.init(getApplicationContext());
		}
	}
	
	public boolean isInitedAddPage() {
		return mInitedAddPage;
	}
}
