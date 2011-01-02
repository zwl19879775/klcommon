/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import android.app.TabActivity;
import android.content.Intent;
import android.content.res.Resources;
import android.os.Bundle;
import android.widget.TabHost;


public class ViewContactTab extends TabActivity {
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// clear the selected contact list first.
		SelectedContact.instance().clear();
		
		Resources res = getResources();
		TabHost tabHost = getTabHost();
		tabHost.addTab(tabHost.newTabSpec("tab1").setIndicator(
				UIUtils.getString(this, R.string.contact_recent),
				res.getDrawable(R.drawable.alarm)).setContent(
				getIntentByTime(Const.RECENT_CONTACT_CNT)));
		tabHost.addTab(tabHost.newTabSpec("tab2").setIndicator(
				UIUtils.getString(this, R.string.contact_all),
				res.getDrawable(R.drawable.countdown)).setContent(
				getIntentByTime(0)));
	}

	private Intent getIntentByTime(int cnt) {
		Intent intent = new Intent(this, ViewContactPage.class);
		intent.putExtra("view_count", cnt);
		return intent;
	}
}
