package com.kl.BlackList;

import android.app.TabActivity;
import android.content.Intent;
import android.os.Bundle;
import android.widget.TabHost;

public class BlackSelectTab extends TabActivity {
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		TabHost tabHost = getTabHost();
		tabHost.addTab(tabHost.newTabSpec("sms_page")
				.setIndicator(UIUtils.getString(this, R.string.tab_sms), null)
				.setContent(new Intent(this, ConversationActivity.class)));
		tabHost.addTab(tabHost.newTabSpec("recent_page")
				.setIndicator(UIUtils.getString(this, R.string.tab_calllog), null)
				.setContent(new Intent(this, CallLogActivity.class)));
		tabHost.addTab(tabHost.newTabSpec("contact_page")
				.setIndicator(UIUtils.getString(this, R.string.tab_contact), null)
				.setContent(new Intent(this, ContactActivity.class)));
	}
}