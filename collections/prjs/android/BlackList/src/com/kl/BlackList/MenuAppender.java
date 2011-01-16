package com.kl.BlackList;

import java.util.List;

import android.view.Menu;
import android.view.MenuItem;

public class MenuAppender {
	public static final int MENU_ADD = Menu.FIRST;
	public static final int MENU_CANCEL = MENU_ADD + 1;

	public interface ListCollector {
		List<String> collect();
	}
	
	private static class InsertHandler implements BlackListCache.OnOperCompleted {
		@Override
		public void onCompleted() {
		}
		
		@Override
		public void onInserted(BlackListCache.Item item) {
			Log.d(String.format("Insert black list %s completed.", item.address));
		}
	}
	
	public static void onCreate(Menu menu) {
        menu.add(0, MENU_ADD, 0, R.string.add_to);
	}
	
	public static void addList(List<String> list) {
		Log.d(String.format("Will add %d blacks.", list.size()));
		BlackListCache.OnOperCompleted handler = new InsertHandler();
		for(String s : list) {
			Log.d("Add " + s);
			BlackListCache.inst().startAsyncInsert(handler, s);
		}
	}
	
	public static boolean onSelected(MenuItem item, ListCollector cl) {
    	switch(item.getItemId()) {
    	case MENU_ADD: {
    		addList(cl.collect());
    		UIUtils.toastInfo(BlackApp.app(), 
    				UIUtils.getString(BlackApp.app(), R.string.add_completed));
    		return true;
    	}
    	case MENU_CANCEL: {
    		UIUtils.toastInfo(BlackApp.app(), "Cancel");
    		return true;
    	}
    	}
    	return false;
	}
}
