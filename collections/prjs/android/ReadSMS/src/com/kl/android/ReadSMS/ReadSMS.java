/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.ArrayList;
import java.util.HashMap;

import android.app.Activity;
import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;

public class ReadSMS extends Activity 
	implements ThreadSmsStore.OnChanged{
	// Assume the list item index is also the mThreadIDs index.
	private ArrayList<String> mThreadIDs = new ArrayList<String>();
	private ArrayList<HashMap<String, Object>> mItemList = new ArrayList<HashMap<String, Object>> (); 
	private boolean mVisible = false;
	private Drawable mDefAvatar = null;
	private static final int MENU_EXPORT_ALL_TXT = Menu.FIRST;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        // maybe initialize all.
        if( AppStatus.checkInit(getApplicationContext()) ) {
        	Log.i("Start a new process, initialize all.");
        }
        else {
        	Log.i("Start from previous process.");
        }
        // load default avatar icon.
        mDefAvatar = getResources().getDrawable(R.drawable.ic_contact_picture);
    	// observe to update UI.
        ThreadSmsStore.instance().setObserver(this);
        Log.d("ReadSMS onCreate, observe on ThreadSmsStore.");
    	updateItemList();
    	setNewSmsListener();
    }
    
    @Override
    protected void onDestroy() {
    	super.onDestroy();
    	// cancel the observer.
    	ThreadSmsStore.instance().setObserver(null);
    	Log.d("ReadSMS onDestroy cancel the observer on ThreadSmsStore.");
    }
    
    @Override
    protected void onStart() {
    	super.onStart();
    	mVisible = true;
    	updateListView();
    }
    
    @Override
    protected void onStop() {
    	super.onStop();
    	mVisible = false;
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);
        menu.add(0, MENU_EXPORT_ALL_TXT, 0, R.string.menu_export_all_text);
        return true;
    }			
    
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
        switch(item.getItemId()) {
            case  MENU_EXPORT_ALL_TXT:
            {
            	if( ExporterManager.instance().exportAll(TxtExporter.EXPORT_TYPE) ){
            		UIUtils.toastInfo(this, UIUtils.getString(this, R.string.export_success));
            	}
            	else {
            		UIUtils.toastInfo(this, UIUtils.getString(this, R.string.export_failed));
            	}
            }
            return true;
        }
        return super.onMenuItemSelected(featureId, item);
    }     
    
    private void updateListView() {
    	ListView view = (ListView) findViewById(R.id.sms_list);
    	QuickContactAdapter adapter = new QuickContactAdapter( this, mItemList,
    			R.layout.thread_list_item,
    			new String[] { "address", "from", "subject", "date", "avatar" },
    			new int[] { R.id.contact_avatar, R.id.from, R.id.subject, R.id.date, R.id.contact_avatar } );
    	
    	view.setAdapter(adapter);
    	view.setOnItemClickListener(new OnItemClickListener() {  
            public void onItemClick(AdapterView<?> arg0, View arg1, int arg2, long arg3) {   
                viewThread(arg2);
            }
    	} );
    }
    
    private void viewThread(int arg) {
    	if(arg >= mThreadIDs.size()) {
    		Log.w(String.format("Invalid item index:%d, ThreadIDs size:%d.", 
    				arg, mThreadIDs.size()));
    		return;
    	}
    	long threadID = Long.valueOf(mThreadIDs.get(arg));
    	Intent intent = new Intent(this, ThreadPage.class);
    	intent.putExtra("threadID", threadID);
    	Log.d(String.format("Start to launch %d thread page.", threadID));
    	startActivity(intent);
    }
    
    private void updateItemList() {
    	class ThreadSmsTraverse implements ThreadSmsStore.Traverser {
    		private ArrayList<HashMap<String, Object>> mList;
    		
    		ThreadSmsTraverse(ArrayList<HashMap<String, Object>> list) { mList = list; }
    		
    		@Override
    		public void onTraverse(ThreadSmsStore.ThreadItem item) {
    			HashMap<String, Object> i = new HashMap<String, Object>();
    			// someone(msg_count)
    			i.put("from", item.mContact.mName + "(" + item.mSmsList.size() + ")");
    			// the first message.
    			i.put("subject", item.mSmsList.get(0).mBody);
    			// date.
    			i.put("date", SmsUtils.getDateDesc(item.recentDate()));
    			// phone number.
    			i.put("address", item.mContact.mobilePhone());
    			// avatar icon.
    			i.put("avatar", item.mContact.getAvatar(getBaseContext(), mDefAvatar));
    			mList.add(i);
    			// build the map from "list item id" to "thread id".
    			mThreadIDs.add(String.valueOf(item.mThreadID));
    		}
    	} ;
    	clear();
    	ThreadSmsTraverse traverser = new ThreadSmsTraverse(mItemList);
    	ThreadSmsStore.instance().tranverse(traverser);	
    }
    
    private void setNewSmsListener() {
    	Button newSms = (Button) findViewById(R.id.write_new_msg);
    	newSms.setOnClickListener(new Button.OnClickListener() {
    		public void onClick(View v) {
    			ReadSMS.this.launchWriteNewSmsPage();
    		}
    	} );
    }
    
    private void launchWriteNewSmsPage() {
    	Intent intent = new Intent(this, ThreadPage.class);
    	intent.putExtra(Const.EXTRA_THREADID, Const.NO_THREADID);
    	Log.d("Start to launch new message page.");
    	startActivity(intent);
    }
    
    private void clear() {
    	mItemList.clear();
    	mThreadIDs.clear();	
    }
    
    @Override
    public void onChanged() {
    	Log.d("ReadSMS SMS thread list changed.");
    	updateItemList();
    	if(mVisible) {
    		Log.d("The ReadSMS is visible, update UI.");
    		updateListView();
    	}
    }
}
