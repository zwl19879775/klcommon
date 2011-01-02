/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.ArrayList;
import java.util.HashMap;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.provider.ContactsContract.QuickContact;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.QuickContactBadge;
import android.widget.SimpleAdapter;


public class ThreadPage extends Activity 
	implements SMSReader.SMSObserver {
	private ThreadSmsStore.ThreadItem mSmsList = null;
	private long mThreadID = Const.NO_THREADID;
	private ArrayList<HashMap<String, String>> mItemList = new ArrayList<HashMap<String, String>> (); 
	private static final int MENU_EXPORT_TXT_ID = Menu.FIRST;
	private static final int REQUEST_CONTACT = 0;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
    	super.onCreate(savedInstanceState);
    	setContentView(R.layout.view_sms);
    	Intent intent = getIntent();
    	long threadID = intent.getLongExtra(Const.EXTRA_THREADID, Const.NO_THREADID);
    	mThreadID = threadID;
    	Log.d(String.format("ThreadPage with threadID:%d.", mThreadID));
    	if(mThreadID != Const.NO_THREADID) {
    		addViewHeader();
    		mSmsList = ThreadSmsStore.instance().get(mThreadID);
    		fillListView();
    	} // otherwise, we want to send a new message.
    	else {
    		addNewHeader();
    	}
    	setSendListener();
    }
    
    @Override
    protected void onStart() {
    	Log.d("ThreadPage onStart.");
    	super.onStart();
    	// register as an observer to SMSReader.
    	SMSReader.instance().registerObserver(this);
    }
    
    @Override
    public void onStop() {
    	Log.d("ThreadPage onStop.");
    	super.onStop();
    	SMSReader.instance().cancelObserver(this);
    }
    
    @Override 
    public void onResume() {
    	Log.d("ThreadPage onResume.");
    	super.onResume();
    }
   
    private void removeHeader() {
    	ViewGroup group = (ViewGroup) findViewById(R.id.view_sms_header);
    	group.removeAllViews();
    }
    
    private void addViewHeader() {
    	ViewGroup group = (ViewGroup) findViewById(R.id.view_sms_header);
    	// inflate views at runtime.
    	LayoutInflater factory = LayoutInflater.from(this);
    	View header = factory.inflate(R.layout.view_sms_header, null);
    	group.addView(header);
    }
    
    private void addNewHeader() {
    	ViewGroup group = (ViewGroup) findViewById(R.id.view_sms_header);
    	// inflate views at runtime.
    	LayoutInflater factory = LayoutInflater.from(this);
    	View header = factory.inflate(R.layout.new_sms_header, null);
    	group.addView(header);
    	setViewContactListener();
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);
        menu.add(0, MENU_EXPORT_TXT_ID, 0, R.string.menu_export_text);
        return true;
    }			
    
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
        switch(item.getItemId()) {
            case MENU_EXPORT_TXT_ID:
            {
            	if(mThreadID != Const.NO_THREADID) {
            		if( ExporterManager.instance().export(TxtExporter.EXPORT_TYPE, mThreadID) ){
            			UIUtils.toastInfo(this, UIUtils.getString(this, R.string.export_success));
            		}
            		else {
            			UIUtils.toastInfo(this, UIUtils.getString(this, R.string.export_failed));
            		}
            	}
            }
            return true;
        }
        return super.onMenuItemSelected(featureId, item);
    }    
    
    private void setViewContactListener() {
    	Button sendBtn = (Button) findViewById(R.id.view_contact);
    	sendBtn.setOnClickListener(new Button.OnClickListener() {
    		public void onClick(View v) {
    			Intent intent = new Intent(ThreadPage.this, ViewContactTab.class);
    			// ForResult does not worked here, the target activity will set data in onStop,
    			// but onActivityResult will be called BEFORE onStop, what wired.
    			ThreadPage.this.startActivityForResult(intent, REQUEST_CONTACT);
    		}
    	}); 	
    }
    
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    	if(requestCode == REQUEST_CONTACT) {
    		Log.d("ThreadPage onActivityResult, Pick contact result.");
    		getSelectedContact();
    	}
    }
    
    private void getSelectedContact() {
    	EditText text = (EditText) findViewById(R.id.new_sms_address);
    	String allContact = SelectedContact.instance().getAllDesc();
    	if(!allContact.equals("")) {
    		text.setText(allContact);
    	}
    }
    
    private void setSendListener() {
    	Button sendBtn = (Button) findViewById(R.id.new_sms_send);
    	sendBtn.setOnClickListener(new Button.OnClickListener() {
    		public void onClick(View v) {
    			EditText msgView = (EditText) ThreadPage.this.findViewById(R.id.new_sms_context);
    			String msg = msgView.getText().toString();
    			if(msg.length()<=0) {
    				UIUtils.toastInfo(getBaseContext(), UIUtils.getString(ThreadPage.this, R.string.notify_no_body));
    			}
    			else {
    				String[] nums = ThreadPage.this.getSendNumber();
    				if(nums == null)  return;
    				SMSSender.sendSMS(getBaseContext(), nums, msg);
    			}
    			msgView.setText("");
    		}
    	}); 	
    }
    
    private String[] getSendNumber() {
    	if(mThreadID == Const.NO_THREADID) {
    		Log.d("New message, use input phone number.");
    		EditText numTxt = (EditText) findViewById(R.id.new_sms_address);
    		String allNums = numTxt.getText().toString();
    		if(allNums.length() <= 0) {
    			UIUtils.toastInfo(getBaseContext(), UIUtils.getString(this, R.string.notify_no_addr));
    			return null;
    		}
    		return SelectedContact.instance().getNumbersFromDesc(allNums);
    	}
    	else {
    		return new String[] { mSmsList.mContact.mobilePhone() };
    	}
    }
    
    private void fillListView() {
    	readAll();
    	updateListView();
		updateQuickBadge(mSmsList.mContact);
    }
    
    private void updateListView() {
    	ListView view = (ListView) findViewById(R.id.sms_detail);
    	SimpleAdapter listItemAdapter = new SimpleAdapter(this, mItemList,    
                R.layout.list_item, 
                new String[] {"context","time"},    
                new int[] {R.id.list_context, R.id.list_tip}   
            );   
    	view.setAdapter(listItemAdapter);
    	view.setSelection(view.getCount()-1); /*scroll down to the bottom */	
    	Log.d("Update list view.");
    }
    
    private ArrayList<HashMap<String, String>> readAll() {
    	ThreadSmsStore.ThreadItem smsAll = mSmsList;
    	if(smsAll != null) {
    		int size = smsAll.mSmsList.size();
    		for( int i = 0; i < size; ++i ) {
    			SMSItem sms = smsAll.mSmsList.get(size-i-1); /*reverse by date*/
    			addNewItem(sms);
    		}
    	}
    	return mItemList;
    }

    private void updateQuickBadge(ContactManager.Contact contact) {
    	QuickContactBadge badge = (QuickContactBadge) findViewById(R.id.thread_contact_avatar);
        badge.setMode(QuickContact.MODE_SMALL);
        Drawable avatar = contact.getAvatar(this, getResources().getDrawable(R.drawable.ic_contact_picture));
        badge.setImageDrawable(avatar);
        badge.assignContactFromPhone(contact.mobilePhone(), true);
    }
    
    private void addNewItem(SMSItem sms) {
		HashMap<String, String> item = new HashMap<String, String>();
		item.put("context", SmsUtils.getMessageDesc(mSmsList.mContact, sms) );
		item.put("time", SmsUtils.getDateDesc(sms.mDate));
		mItemList.add(item);    	
    }
    
    public void onNewItem(Context context, SMSItem sms) {
    	if(sms.mThreadID != mThreadID && 
    		mThreadID != Const.NO_THREADID) return;
    	Log.d("ThreadPage got a new message.");
    	if(mThreadID == Const.NO_THREADID) {
    		Log.d("ThreadPage got 1st message.");
    		mThreadID = sms.mThreadID;
    		mSmsList = ThreadSmsStore.instance().get(mThreadID);
    		/* the number maybe exist already. so here should read all. */
    		readAll();
    		/* change the view */
    		removeHeader();
    		addViewHeader();
    		updateQuickBadge(mSmsList.mContact);
    	}
    	else {
    		addNewItem(sms);
    	}
    	updateListView();
    }
}
 