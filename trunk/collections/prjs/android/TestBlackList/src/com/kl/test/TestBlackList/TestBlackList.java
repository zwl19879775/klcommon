package com.kl.test.TestBlackList;
 
import android.app.Activity;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ListView;
import android.widget.SimpleCursorAdapter;

import com.kl.android.BlackListProvider.BlackList;

public class TestBlackList extends Activity {
	private final static int MENU_INSERT = Menu.FIRST;
	private final static int MENU_DELETE = MENU_INSERT + 1;
	private final static int MENU_UPDATE = MENU_DELETE + 1;
	private SimpleCursorAdapter mAdapter;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
    	mAdapter = new SimpleCursorAdapter(this, android.R.layout.simple_list_item_2,
    		null, new String[] { BlackList.ID, BlackList.ADDRESS },
    		new int[] { android.R.id.text1, android.R.id.text2 } );
        fillListView();
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);
        menu.add(0, MENU_INSERT, 0, "Test insert");
        menu.add(0, MENU_DELETE, 0, "Test delete");
        menu.add(0, MENU_UPDATE, 0, "Test update");
        return true;
    }			
    
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
        switch(item.getItemId()) {
            case MENU_INSERT:
            {
            	testInsert();
            	fillListView();
            }
            return true;
            
            case MENU_DELETE:
            {
            	testDelete();
            	fillListView();
            }
            return true;
            
            case MENU_UPDATE:
            {
            	testUpdate();
            	fillListView();
            }
            return true;
        }
        return super.onMenuItemSelected(featureId, item);
    }     
    
    private void testInsert() {
    	ContentResolver cr = getContentResolver();
    	ContentValues v1 = new ContentValues();
    	v1.put(BlackList.ADDRESS, "123");
    	cr.insert(BlackList.CONTENT_URI, v1);
    	
    	ContentValues v2 = new ContentValues();
    	v2.put(BlackList.ADDRESS, "456");
    	v2.put(BlackList.TYPE, BlackList.TYPE_MMS);
    	cr.insert(BlackList.CONTENT_URI, v1);
    }
    
    private void testDelete() {
    	ContentResolver cr = getContentResolver();
    	cr.delete(BlackList.CONTENT_URI, null, null);
    }
    
    private void testUpdate() {
    	ContentValues v1 = new ContentValues();
    	v1.put(BlackList.ADDRESS, "123456");
    	Uri uri = Uri.withAppendedPath(BlackList.CONTENT_URI, "1");
    	getContentResolver().update(uri, v1, null, null);
    }
    
    private void fillListView() {
    	ListView list = (ListView) findViewById(R.id.list);
    	Cursor cur = getContentResolver().query(BlackList.CONTENT_URI, null, null, null, null);
    	mAdapter.changeCursor(cur);
    	list.setAdapter(mAdapter);
    }
}