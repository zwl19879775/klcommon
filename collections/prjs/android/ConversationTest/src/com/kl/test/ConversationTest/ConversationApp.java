package com.kl.test.ConversationTest;

import android.app.Application;

public class ConversationApp extends Application {
	private static Application mApp;
	
	@Override
	public void onCreate() {
		super.onCreate();
		mApp = this;
		Log.init("TestConver");
		Log.d("Application startup");
		RecipientIdCache.init(this);
	}
	
	public static Application app() {
		return mApp;
	}
}
