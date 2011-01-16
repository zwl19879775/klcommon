package com.kl.BlackList;

import com.kl.android.BlackListProvider.BlackList;
import android.app.ListActivity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ListView;
import android.widget.TextView;

public class ViewLogActivity extends ListActivity {
	public static final String BLACKID_ARG = "blackid";
	public static final long INVALID_ID = 0;
	private class ViewBinder implements CustomListAdapter.ViewBinder, 
		BlockLog.OnOperCompleted {

		@Override
		public void setViewValue(View view, int pos) {
			BlockLog.Item log = getLog(pos);
			if(log == null) {
				TextView desc = (TextView) view.findViewById(R.id.view_log_desc);
				desc.setText(UIUtils.getString(getBaseContext(), R.string.no_log_entry));
			}
			else {
				TextView desc = (TextView) view.findViewById(R.id.view_log_desc);
				desc.setText(log.desc);
				TextView date = (TextView) view.findViewById(R.id.view_log_date);
				date.setText(UIUtils.getDateFormat(log.date));
				TextView type = (TextView) view.findViewById(R.id.view_log_type);
				type.setText(formatType(log.type));
			}
		}

		@Override
		public int valueSize() {
			return getLogCount();
		}
		
		@Override
		public void onCompleted(boolean ret) {
			mAdapter.notifyDataSetChanged();
		}
		
		private String formatType(long type) {
			if(type == BlackList.BlockLog.TYPE_MMS) {
				return UIUtils.getString(getBaseContext(), R.string.block_mms);
			}
			return UIUtils.getString(getBaseContext(), R.string.block_phone);
		}
	}
	
	private CustomListAdapter mAdapter;
	private ViewBinder mBinder;
	private long mBlackID;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);	
		Log.d("ViewLogActivity onCreate.");
		retrieveBlackID();
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
        return true;
    }			
    
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
        return super.onMenuItemSelected(featureId, item);
    } 
    
    private void retrieveBlackID() {
    	Intent intent = getIntent();
    	mBlackID = intent.getLongExtra(BLACKID_ARG, INVALID_ID);
    }
    
    private int getLogCount() {
    	if(mBlackID == INVALID_ID)  return 0;
    	BlockLog.BlackLogItem logs = BlockLog.inst().get(mBlackID);
    	if(logs == null || logs.count() == 0) {
    		return 1; // add a 'no log entry' item.
    	}
    	return logs.count(); 
    }
    
    private BlockLog.Item getLog(int pos) {
    	BlockLog.BlackLogItem logs = BlockLog.inst().get(mBlackID);
    	if(logs == null || logs.count() == 0) {
    		return null;
    	}
    	BlockLog.Item log = logs.logs.get(pos);
    	return log;
    }
    
	private void setUIProperty() {
		//setContentView(R.layout.view_log_list);
		final ListView listView = getListView();
		listView.setItemsCanFocus(false);
		mBinder = new ViewBinder();
		mAdapter = new CustomListAdapter(this, mBinder, R.layout.view_log_list_item);
	}

	private void fillListView() {
		setListAdapter(mAdapter);
		if(mBlackID != INVALID_ID) {
			BlockLog.inst().startAsyncQuery(mBinder, mBlackID);
		}
	}	
}
	