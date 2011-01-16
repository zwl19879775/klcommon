package com.kl.BlackList;

import java.util.ArrayList;
import java.util.List;
import android.app.ListActivity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.util.SparseBooleanArray;
import android.view.ContextMenu;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.View.OnCreateContextMenuListener;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;

public class ConversationActivity extends ListActivity {
	private static final Uri VIEW_SMS_URI = Uri.parse("content://mms-sms/conversations");
	private static final int MENU_VIEW = 10;
	private static final int MENU_SELECT = 11;
	
	/// Handle list view item stuff.
	private class ViewBinder extends CheckBoxBinder implements AbsListView.RecyclerListener {
		public ViewBinder() {
			super(R.id.list_checkbox);
		}
		
		public void setViewValue(View view, int pos) {
			super.setViewValue(view, pos);
			ConversationListItem convItem = (ConversationListItem) view;
			Conversation.Item conv = mConversations.get(pos);
			convItem.bind(conv);
		}
		
		public int valueSize() {
			return mConversations.size();
		}
		
	    @Override
	    public void onMovedToScrapHeap(View view) {
			ConversationListItem convItem = (ConversationListItem) view;
			convItem.unbind();
	    }
	}
	
    private class ContextMenuListener implements OnCreateContextMenuListener {   
    	@Override
        public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {   
    		menu.add(0, MENU_VIEW, 0, R.string.view_sms);
    	}   
    }
    
	private class Traverser implements Conversation.OnTraverse {
		@Override
		public void onTraverse(Conversation.Item item) {
			addConversation(item);
		}
	}
	
	private class QueryHandler implements Conversation.OnQueryCompleted {
		@Override
		public void onCompleted(boolean success) {
			if(!success) {
				Log.w("Query conversation failed.");
				return;
			}
			onQueryCompleted();
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
			for(int i = 0; i < mConversations.size(); ++i) {
				if(!sels.get(i)) continue;
				Conversation.Item conv = mConversations.get(i);
				Contact.Item contact = ContactBridge.get(conv.id);
				// only get the 1st number.
				if(contact.phoneCount() > 0) {
					list.add(contact.phones.get(0).number);
				}
			}
			return list;
		}
	}
	
	private ViewBinder mBinder;
	private CustomListAdapter mAdapter;
	private List<Conversation.Item> mConversations;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d("ConversationActivity onCreate");
        getListView().setChoiceMode(ListView.CHOICE_MODE_MULTIPLE);			
    	mBinder = new ViewBinder();
    	mAdapter = new CustomListAdapter(this, mBinder, R.layout.conversation_list_item);
    	mConversations = new ArrayList<Conversation.Item>();
    }
    
    @Override
    public void onStart() {
    	super.onStart();
        fillListView();
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);
        MenuAppender.onCreate(menu);
        return true;
    }			
    
    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
    	Log.d("ConversationActivity onMenuItemSelected");
    	if(MenuAppender.onSelected(item, new NumberCollector())) return true;
        return super.onMenuItemSelected(featureId, item);
    } 
    
    @Override
    public boolean onContextItemSelected(MenuItem item) { 
    	AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) 
    		item.getMenuInfo();
    	switch(item.getItemId()) {
    	case MENU_VIEW:
    		viewSMS(info.position);
    		break;
    	case MENU_SELECT:
    		break;
    	}
        return super.onContextItemSelected(item);
    }
    
    private void viewSMS(int pos) {
    	Conversation.Item conv = mConversations.get(pos);
    	Uri uri = Uri.withAppendedPath(VIEW_SMS_URI, String.valueOf(conv.id));
    	Intent intent = new Intent(Intent.ACTION_VIEW);
    	intent.setData(uri);
    	startActivity(intent);
    }
    
    private void fillListView() {
    	ListView self = getListView();
		self.setOnCreateContextMenuListener(new ContextMenuListener());
    	setListAdapter(mAdapter);
    	self.setOnItemClickListener(new ClickListener());
    	Conversation.inst().startAsyncQuery(new QueryHandler());
    }
    
    private void addConversation(Conversation.Item item) {
    	mConversations.add(item);
    }
    
    private void onQueryCompleted() {
    	Log.d("Conversation query completed.");
    	Conversation.inst().tranverse(new Traverser(), null);
    	// update the list view.
    	mAdapter.notifyDataSetChanged();
    }
}
