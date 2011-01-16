package com.kl.BlackList;

import java.util.ArrayList;
import java.util.List;
import android.app.ListActivity;
import android.os.Bundle;
import android.util.SparseBooleanArray;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.TextView;

public class CallLogActivity extends ListActivity {
	private class ViewBinder extends CheckBoxBinder implements CallLog.OnQueryCompleted {
		public ViewBinder() {
			super(R.id.calllog_checkbox);
		}

		public void setViewValue(View view, int pos) {
			super.setViewValue(view, pos);
			CallLog.Item log = CallLog.inst().get(pos);
			TextView name = (TextView) view.findViewById(R.id.calllog_name);
			name.setText(log.cachedName);
			TextView number = (TextView) view.findViewById(R.id.calllog_number);
			number.setText(String.format("%s[ %s ]", log.number, getTypeDesc(log.type)));
			TextView date = (TextView) view.findViewById(R.id.calllog_date);
			date.setText(UIUtils.getDateFormat(log.date));
		}

		public int valueSize() {
			return CallLog.inst().getLogs().size();
		}

		@Override
		public void onCompleted() {
		}
		
		@Override
		public void onOneCompleted(CallLog.Item item) {
			mAdapter.notifyDataSetChanged();
		}
		
		private String getTypeDesc(int type) {
			switch(type) {
			case CallLog.TYPE_INCOMING:
				return UIUtils.getString(getBaseContext(), R.string.calllog_incoming);
			case CallLog.TYPE_MISSED:
				return UIUtils.getString(getBaseContext(), R.string.calllog_missed);
			case CallLog.TYPE_OUTGOING:
				return UIUtils.getString(getBaseContext(), R.string.calllog_outgoing);
			}
			return "";
		}
	}

	private class ClickListener implements OnItemClickListener {
		@Override
		public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
			mBinder.toggle(view, position);
		}
	}
	
	private class NumberCollector implements MenuAppender.ListCollector {
		@Override
		public List<String> collect() {
			List<String> list = new ArrayList<String>();
			final ListView listView = getListView();
			int size = CallLog.inst().getLogs().size();
			SparseBooleanArray sels = listView.getCheckedItemPositions();
			for(int i = 0; i < size; ++i) {
				if(!sels.get(i)) continue;
				CallLog.Item log = CallLog.inst().get(i);
				list.add(log.number);
			}
			return list;
		}
	}
	
	private CustomListAdapter mAdapter;
	private ViewBinder mBinder;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);	
		Log.d("CallLogActivity onCreate.");
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
        MenuAppender.onCreate(menu);
        return true;
    }			
    
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
    	Log.d("CallLogActivity onMenuItemSelected");
    	if(MenuAppender.onSelected(item, new NumberCollector())) return true;
        return super.onMenuItemSelected(featureId, item);
    } 
    
	private void setUIProperty() {
		final ListView listView = getListView();
		listView.setItemsCanFocus(false);
		listView.setChoiceMode(ListView.CHOICE_MODE_MULTIPLE);			
		listView.setOnItemClickListener(new ClickListener());
		mBinder = new ViewBinder();
		mAdapter = new CustomListAdapter(this, mBinder, R.layout.calllog_list_item);
	}

	private void fillListView() {
		setListAdapter(mAdapter);
		CallLog.inst().startAsyncQuery(mBinder);
	}	
}
	