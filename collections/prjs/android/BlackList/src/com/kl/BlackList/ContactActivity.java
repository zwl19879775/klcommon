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

public class ContactActivity extends ListActivity {
	
	private class ViewBinder extends CheckBoxBinder implements Contact.OnLoadPhone {
		public ViewBinder() {
			super(R.id.contact_checkbox);
		}
		
		public void setViewValue(View view, int pos) {
			super.setViewValue(view, pos);
			ContactItem contact = mContacts.get(pos);
			TextView name = (TextView) view.findViewById(R.id.contact_name);
			name.setText(contact.displayName);
			TextView number = (TextView) view.findViewById(R.id.contact_number);
			number.setText(contact.number);
		}
		
		public int valueSize() {
			return mContacts.size();
		}
	    
	    @Override
	    public void onLoad(Contact.Item contact, Contact.Phone phone) {
	    	addContact(contact, phone.number);
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
			SparseBooleanArray sels = listView.getCheckedItemPositions();
			for(int i = 0; i < mContacts.size(); ++i) {
				if(!sels.get(i)) continue;
				ContactItem contact = mContacts.get(i);
				list.add(contact.number);
			}
			return list;
		}
	}
	
	private final class ContactItem {
		String displayName;
		String number;
	}

	private List<ContactItem> mContacts;
	private CustomListAdapter mAdapter;
	private ViewBinder mBinder;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);	
        Log.d("ContactActivity onCreate.");
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
    	Contact.inst().setPhoneLoadObserver(null);
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);
        MenuAppender.onCreate(menu);
        return true;
    }			 
    
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
    	Log.d("ContactActivity onMenuItemSelected");
    	if(MenuAppender.onSelected(item, new NumberCollector())) return true;
        return super.onMenuItemSelected(featureId, item);
    } 
    
	private void setUIProperty() {
        final ListView listView = getListView();
        listView.setItemsCanFocus(false);
        listView.setChoiceMode(ListView.CHOICE_MODE_MULTIPLE);			
    	listView.setOnItemClickListener(new ClickListener());
        mContacts = new ArrayList<ContactItem>();
        mBinder = new ViewBinder();
        mAdapter = new CustomListAdapter(this, mBinder, R.layout.contact_list_item);
	}
	
	private void fillListView() {
        setListAdapter(mAdapter);
        Contact.inst().setPhoneLoadObserver(mBinder);
        Contact.inst().startAsyncQuery();
	}
	
	private void addContact(Contact.Item contact, String number) {
		ContactItem item = new ContactItem();
		item.displayName = contact.displayName;
		item.number = number;
		mContacts.add(item);
		mAdapter.notifyDataSetChanged();
	}
}
