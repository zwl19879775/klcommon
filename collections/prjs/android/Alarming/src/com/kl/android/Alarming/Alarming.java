package com.kl.android.Alarming;

import java.util.ArrayList;
import java.util.HashMap;
import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnCreateContextMenuListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.SimpleAdapter;
import android.widget.Toast;


public class Alarming extends Activity {
	private static final int CONTEXT_M_DEL = Menu.FIRST;
	private static final int CONTEXT_M_CANCEL = CONTEXT_M_DEL + 1;
	private static final int INSERT_ID = CONTEXT_M_CANCEL + 1;
	private static final int EXIT_ID = INSERT_ID + 1;
	private Config mCfg;
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);
		initListView();
		mCfg = Config.instance();
		mCfg.init(this);
		fillData();
		Log.d(ConfigData.LOGTAG, "main activity onCreate.");
	}
    private void fillData() {
    	ArrayList<HashMap<String, Object>> listItem = new ArrayList<HashMap<String, Object>>();  
    	int cnt = mCfg.Count();
    	for( int i = 0; i < cnt; ++ i) {
    		HashMap<String, Object> map = new HashMap<String, Object>();   
    		mCfg.fillItem(i, map);
            listItem.add(map);  
    	}
    	SimpleAdapter listItemAdapter = new SimpleAdapter(this,listItem,    
                R.layout.list_item, 
                new String[] {"enable_id","ItemTitle", "ItemText"},    
                new int[] {R.id.enable_id,R.id.ItemTitle,R.id.ItemText}   
            );   

    	ListView view = (ListView) findViewById(R.id.alarm_list);
    	view.setAdapter(listItemAdapter);
    }
    private void initListView() {
    	ListView list = (ListView) findViewById(R.id.alarm_list);
    	list.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
        list.setOnItemClickListener(new OnItemClickListener() {  
            public void onItemClick(AdapterView<?> arg0, View arg1, int arg2, long arg3) {   
            
                launchSetting(arg2);
            }   
        });   
           
        list.setOnCreateContextMenuListener(new OnCreateContextMenuListener() {   
            public void onCreateContextMenu(ContextMenu menu, View v,ContextMenuInfo menuInfo) {   
                menu.setHeaderTitle("ContextMenu");      
                menu.add(0, CONTEXT_M_DEL, 0, R.string.menu_del);   
                menu.add(0, CONTEXT_M_CANCEL, 0, R.string.menu_cancel);      
            }   
        });    
    }
    public boolean onContextItemSelected(MenuItem item) {   
        switch(item.getItemId()) {
        	case CONTEXT_M_DEL:
        	{        		
        	}
        }
        return super.onContextItemSelected(item);   
    }  
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);
        menu.add(0, INSERT_ID, 0, R.string.menu_insert);
        menu.add(0, EXIT_ID, 0, R.string.menu_exit);
        return true;
    }
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
        switch(item.getItemId()) {
            case INSERT_ID:
            {
            	SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this); 
            	int index = mCfg.addItem(settings);
            	launchSetting(index);
            }
            return true;
            case EXIT_ID:
            {
            	finish();
            }
            return true;
        }
        return super.onMenuItemSelected(featureId, item);
    }
    
    private void launchSetting( int index ) {
    	int arg = 0;
    	SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this);   
    	mCfg.begin(index, settings);
    	startActivityForResult(new Intent(this, Setting.class), arg);   
    }

    protected  void onActivityResult(int requestCode, int resultCode, Intent data) {   
    	if( data != null ) {
    		Bundle extras = data.getExtras();
    		Long op = extras.getLong(Setting.OP_KEY);
    		long t = op.longValue();
    		if( t == Setting.OP_DEL ) {
    			mCfg.delItem();
    			fillData();
    			return;
    		}
    	}
    	else {
    		int index = mCfg.operIndex();
    		SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this);   
    		mCfg.end(settings);
        	fillData();    	
        	// schedule.
        	scheduleAlarm(index);
    	}
    }
    
    private boolean scheduleAlarm( int index ) {
    	ConfigData cfg = mCfg.retriveCfg(index);
    	if( cfg.mEnableFlag ) {
    		return AlarmHandler.instance().schedule(cfg, this, index, false);
    	}
    	else
    	{
    		AlarmHandler.instance().cancel(this, index);
    		Toast.makeText(this, "The alarm [" + cfg.mLabel + "] has been canceled.", 
    				Toast.LENGTH_LONG ).show();
    		return true;
    	}
    }
}
