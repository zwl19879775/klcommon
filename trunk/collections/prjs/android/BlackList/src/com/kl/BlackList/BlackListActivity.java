package com.kl.BlackList;

import android.app.ListActivity;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnCreateContextMenuListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.TextView;

public class BlackListActivity extends ListActivity {
	private static final int MENU_VIEW = 1;
	private static final int MENU_DEL = 2;
	private static final int MENU_NEW = 3;
	private static final int MENU_DELALL = 4;
	
	private class ViewBinder implements CustomListAdapter.ViewBinder,
		BlackListCache.OnOperCompleted {
		@Override
		public void setViewValue(View view, int pos) {
			BlackListCache.Item b = BlackListCache.inst().get(pos);
			TextView address = (TextView) view.findViewById(R.id.black_address);
			address.setText(formatAddress(b));
			TextView date = (TextView) view.findViewById(R.id.black_date);
			date.setText(UIUtils.getDateFormat(b.date));
			TextView latestLog = (TextView) view.findViewById(R.id.black_log);
			latestLog.setText(TextUtils.isEmpty(b.latestLog) ? 
					UIUtils.getString(getBaseContext(), R.string.no_log_entry):b.latestLog);
		}

		private String formatAddress(BlackListCache.Item b) {
			if(b.logCount > 0) {
				return String.format("%s(%d)", b.address, b.logCount);
			}
			return b.address;
		}
		
		@Override
		public int valueSize() {
			return BlackListCache.inst().size();
		}
		
		@Override
		public void onCompleted() {
			mAdapter.notifyDataSetChanged();
		}
		
		@Override
		public void onInserted(BlackListCache.Item item) {
		}
	}
	
    private class ContextMenuListener implements OnCreateContextMenuListener {   
    	@Override
        public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {   
    		AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) menuInfo;
    		if (info.position > 0) { 
    			menu.add(0, MENU_VIEW, 0, R.string.view_log);
    			menu.add(0, MENU_DEL, 0, R.string.remove);
    		}
        }   
    }
    
	private class ClickListener implements OnItemClickListener {
		@Override
		public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
			if(position == 0) {
				openAddNewActivity();
			}
			else {
				Log.d(String.format("BlackList item %d has been clicked.", position));
				openViewLogActivity(position);
			}
		}
	}
	
	private CustomListAdapter mAdapter;
	private ViewBinder mBinder;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);	
		Log.d("BlackListActivity onCreate.");
		createHeaderView();
		setUIProperty();
	}

	@Override
	public void onStart() {
		super.onStart();
		fillListView();
	}

	@Override
	public void onStop() {
		super.onStop();
	}
	
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);
        menu.add(0, MENU_NEW, 0, R.string.add);
        menu.add(0, MENU_DELALL, 0, R.string.remove_all);
        return true;
    }			
    
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
    	switch(item.getItemId()) {
    	case MENU_NEW:
    		openAddNewActivity();
    		return true;
    	case MENU_DELALL:
    		removeAll();
    		return true;
    	}
        return super.onMenuItemSelected(featureId, item);
    } 
    
    @Override
    public boolean onContextItemSelected(MenuItem item) { 
    	AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) 
    		item.getMenuInfo();
    	long pos = info.position - 1; // excluding the header view.
    	switch(item.getItemId()) {
    	case MENU_VIEW:
    		break;
    	case MENU_DEL:
    		removeBlack(pos);
    		break;
    	}
        return super.onContextItemSelected(item);
    }
    
    private void createHeaderView() {
    	ListView listView = getListView();
        LayoutInflater inflater = LayoutInflater.from(this);
        View headerView = inflater.inflate(R.layout.black_list_header, listView, false); 	
        listView.addHeaderView(headerView, null, true);
    }
    
	private void setUIProperty() {
		final ListView listView = getListView();
		listView.setItemsCanFocus(false);
		listView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
		listView.setOnItemClickListener(new ClickListener());
		listView.setOnCreateContextMenuListener(new ContextMenuListener());
		mBinder = new ViewBinder();
		mAdapter = new CustomListAdapter(this, mBinder, R.layout.black_list_item);
	}

	private void fillListView() {
		setListAdapter(mAdapter);
		BlackListCache.inst().startAsyncQuery(mBinder);
	}	
	
	private void openAddNewActivity() {
		if(!BlackApp.app().isInitedAddPage() ) {
			BlackApp.app().initAddPageData();
		}
		startActivity(new Intent(BlackListActivity.this, BlackSelectTab.class));
	}
	
	private void openViewLogActivity(long pos) {
		pos = pos - 1;
		BlackListCache.Item black = BlackListCache.inst().get((int)pos);
		BlackApp.app().checkInitBlockLog();
		Intent intent = new Intent(this, ViewLogActivity.class);
		intent.putExtra(ViewLogActivity.BLACKID_ARG, black.id);
		startActivity(intent);
	}
	
	private void removeBlack(long pos) {
		BlackListCache.Item item = BlackListCache.inst().get((int)pos);
		if(item != null) {
			BlackListCache.inst().startAsyncDelete(mBinder, item.id);
		}
	}
	
	private void removeAll() {
		BlackListCache.inst().startAsyncDelete(mBinder, BlackListCache.IDALL);
	}
}
	