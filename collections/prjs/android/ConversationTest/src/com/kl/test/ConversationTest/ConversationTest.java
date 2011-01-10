package com.kl.test.ConversationTest;

import android.app.Activity;
import android.content.AsyncQueryHandler;
import android.content.ContentResolver;
import android.database.Cursor;
import android.os.Bundle;
import android.widget.ListView;

/*
 Use ListActivity can make the list view scroll more smoothly.
*/
public class ConversationTest extends Activity {
	private static final int QUERY_ALL_TOKEN = 1;
	private ConversationListAdapter mAdapter;
	
	private class ConversationQueryHandler extends AsyncQueryHandler {
		public ConversationQueryHandler(ContentResolver contentResolver) {
            super(contentResolver);
        }
		
        @Override
        protected void onQueryComplete(int token, Object cookie, Cursor cursor) {		
        	switch(token) {
        	case QUERY_ALL_TOKEN:
        		onQueryAllComplete(cursor);
        		break;
        	}
        }
        
        private void onQueryAllComplete(Cursor cursor) {
        	Log.d(String.format("async query completed:%d", System.currentTimeMillis()));
        	// this may cost many time too.
        	mAdapter.changeCursor(cursor);
        	Log.d(String.format("query data completed:%d", System.currentTimeMillis()));
        	ConversationTest.this.setTitle("Query Completed.");
        }
	}
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    	setTitle("Querying");
        setContentView(R.layout.main);
        fillListView();
    }
    
    private void fillListView() {
    	ListView view = (ListView) findViewById(R.id.list);
    	Conversation.asyncQueryAll(new ConversationQueryHandler(getContentResolver()), QUERY_ALL_TOKEN);
    	Log.d(String.format("start async query:%d", System.currentTimeMillis()));
    	mAdapter = new ConversationListAdapter(this, null);
    	view.setAdapter(mAdapter);
    	view.setRecyclerListener(mAdapter);
    }
}
