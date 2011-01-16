package com.kl.BlackList;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.RelativeLayout;
import android.widget.TextView;

public class ConversationListItem extends RelativeLayout 
	implements Contact.OnItemUpdate {
	private TextView mFrom;
	private TextView mSnippet;
	private TextView mDate;
	private Conversation.Item mConv;
	private Contact.Item mContact;
	
    public ConversationListItem(Context context) {
        super(context);
    }
    
    public ConversationListItem(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    @Override
    public void onUpdate(Contact.Item item) {
    	updateView();
    }
    
    @Override
    protected void onFinishInflate() {
        super.onFinishInflate();
        mFrom = (TextView) findViewById(R.id.from);
        mSnippet = (TextView) findViewById(R.id.snippet);
        mDate = (TextView) findViewById(R.id.conversation_date);
    }
    
    public void bind(Conversation.Item conv) {
    	mConv = conv;
    	mContact = ContactBridge.bind(mConv, this);
    	mContact.addListener(this);
    	updateView();
    }
    
    public void unbind() {
    	mContact.removeListener(this);
    }
    
    private void updateView() {
    	mFrom.setText(getFromDesc());
    	mSnippet.setText(mConv.snippet);
    	mDate.setText(UIUtils.getDateFormat(mConv.date));
    }
    
    private String getFromDesc() {
    	return String.format("%s(%d)", mContact.displayName, mConv.msgCnt);
    }
}

