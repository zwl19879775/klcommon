/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.ArrayList;
import java.util.List;

import android.app.ListActivity;
import android.content.Intent;
import android.os.Bundle;
import android.util.SparseBooleanArray;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import com.kl.android.ReadSMS.ContactManager.Contact;

public class ViewContactPage extends ListActivity {
	private List<String> mList = new ArrayList<String>();
	/// Display string will split the name and the number by '\n'.
	private List<String> mDisplayList = new ArrayList<String>();
	private int mViewCount = 0;
	/// For performance consider.
	private boolean mDirty = false;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {
    	super.onCreate(savedInstanceState);
    	Log.d(String.format( "ViewContactPage onCreate. %d.", mViewCount));
    	Intent intent = getIntent();
    	mViewCount = intent.getIntExtra("view_count", 0);
    	fillListView();
    	setUIProperty();
    }
	
	@Override
	protected void onPause() {
		super.onPause();
		Log.d("ViewContactPage onPause.");
		// for some FUCKING reasons, i was forced to put the code here.
		if(mDirty) {
			collectSelected();
		}
	}
	
	private void fillListView() {
		readAll();
        setListAdapter(new ArrayAdapter<String>(this,
                android.R.layout.simple_list_item_multiple_choice, mDisplayList));
	}
	
	private void setUIProperty() {
        final ListView listView = getListView();
        listView.setItemsCanFocus(false);
        listView.setChoiceMode(ListView.CHOICE_MODE_MULTIPLE);			
	}
	
	private void collectSelected() {
		final ListView listView = getListView();
		SparseBooleanArray sels = listView.getCheckedItemPositions();
		Log.d("Sel count : " + String.valueOf(sels.size()));
		for(int i = 0; i < mList.size(); ++i) {
			if(sels.get(i)) {
				SelectedContact.instance().add( mList.get(i) );
			}
		}
	}
	
	private void readAll() {
		ContactManager inst = ContactManager.instance();
		inst.checkQuery(this);
		inst.traverse( new ContactManager.Traverser() {
			@Override
			public void onTraverse(Contact contact) {
				ViewContactPage.this.readContact(contact);
			}
		}, mViewCount );
		Log.d("Read contact count:" + String.valueOf(mViewCount));
	}
	
	private void readContact(ContactManager.Contact contact) {
		for( int i = 0; i < contact.phoneCount(); ++i ) {
			ContactManager.PhoneNum num = contact.mPhones.get(i);
			String numDesc = "<" + num.mNumber + ">";
			String s = contact.mName + numDesc;
			String ds = contact.mName + "\n" + numDesc;
			mList.add(s);
			mDisplayList.add(ds);
		}
	}
	
	protected void onListItemClick(ListView l, View v, int pos, long id) {
		super.onListItemClick(l, v, pos, id);
		mDirty = true;
	}
}
