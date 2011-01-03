package com.kl.test.ConversationTest;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.provider.ContactsContract.QuickContact;
import android.util.AttributeSet;
import android.widget.QuickContactBadge;
import android.widget.RelativeLayout;
import android.widget.TextView;

public class ConversationListItem extends RelativeLayout 
	implements ContactManager.OnUpdated {
	private TextView mFrom;
	private TextView mSnippet;
	private TextView mDate;
	private QuickContactBadge mBadge;
	private Conversation mConv;
	private Handler mHandler = new Handler();
	private static Drawable sDefaultAvatar;
	
    public ConversationListItem(Context context) {
        super(context);
        if(sDefaultAvatar == null) {
        	sDefaultAvatar = context.getResources().getDrawable(R.drawable.ic_contact_picture);
        }
    }

    public ConversationListItem(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    @Override
    protected void onFinishInflate() {
        super.onFinishInflate();
        mFrom= (TextView) findViewById(R.id.from);
        mSnippet = (TextView) findViewById(R.id.snippet);
        mDate = (TextView) findViewById(R.id.date);
        mBadge = (QuickContactBadge) findViewById(R.id.contact_avatar);
    }
    
    public void bind(final Conversation conv) {
    	mConv = conv;
    	mConv.mContact.setListener(this);
    	updateView();
    }
    
    public void unbind() {
    	mConv.mContact.setListener(null);
    }
    
    private void updateView() {
    	mFrom.setText(getFrom());
    	mSnippet.setText(mConv.mSnippet);
    	mDate.setText(mConv.getDateDesc());
    	mFrom.setText(getFrom()); 	
    	mBadge.setMode(QuickContact.MODE_SMALL);
    	final ContactManager.Contact contact = mConv.mContact;
    	mBadge.setImageDrawable(contact.getAvatar(getContext(), sDefaultAvatar));
    	mBadge.assignContactFromPhone(contact.mobilePhone(), true);
    }
    
    private String getFrom() {
    	final ContactManager.Contact contact = mConv.mContact;
    	return contact.mName + "(" + mConv.getCount() + ")";
    }
    
    public void onUpdate(ContactManager.Contact contact) {
    	Log.i("Contact item update:" + mConv.mContact.mName);
    	mHandler.post(new Runnable() {
    		public void run() {
    			// update UI.
    			updateView();
    		}
    	});
    }
}
