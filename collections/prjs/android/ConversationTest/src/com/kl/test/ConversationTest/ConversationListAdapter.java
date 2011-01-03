package com.kl.test.ConversationTest;

import android.content.Context;
import android.database.Cursor;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.CursorAdapter;

public class ConversationListAdapter extends CursorAdapter
	implements AbsListView.RecyclerListener {
    private final LayoutInflater mFactory;
    
    public ConversationListAdapter(Context context, Cursor cursor) {
        super(context, cursor, false);
        mFactory = LayoutInflater.from(context);
    }

    @Override
    public void bindView(View view, Context context, Cursor cursor) {
    	Conversation conv = Conversation.from(context, cursor);
    	ConversationListItem convView = (ConversationListItem) view;
    	convView.bind(conv);
    }

    @Override
    public void onMovedToScrapHeap(View view) {
    	ConversationListItem cli = (ConversationListItem) view;
    	cli.unbind();
    }

    @Override
    public View newView(Context context, Cursor cursor, ViewGroup parent) {
        return mFactory.inflate(R.layout.conversation_list_item, parent, false);
    }
}
