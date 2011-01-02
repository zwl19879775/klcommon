/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.List;
import java.util.Map;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.provider.ContactsContract.QuickContact;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.QuickContactBadge;
import android.widget.TextView;


public class QuickContactAdapter extends BaseAdapter {
	public static final int RES_BADGE_ID = 0;
	public static final int RES_FROM_ID = 1;
	public static final int RES_SUBJECT_ID = 2;
	public static final int RES_DATE_ID = 3;
	public static final int RES_AVATAR_ID = 4;
	private LayoutInflater mInflater;	
    private List<? extends Map<String, ?>> mData;
    private String[] mFrom;
    private int[] mTo;
    private int mResource;
   
    public final class ViewHolder{
        public QuickContactBadge badge;
        public TextView from;
        public TextView subject;
        public TextView date;
    }
     
    public QuickContactAdapter(Context context, List<? extends Map<String, ?>> list, int resource,
    		String[] from, int[] to) {
    	mInflater = LayoutInflater.from(context);
    	mFrom = from;
    	mData = list;
    	mTo = to;
    	mResource = resource;
    }
    
    @Override
    public int getCount() {
        return mData.size();
    }

    @Override
    public Object getItem(int arg0) {
        return null;
    }

    @Override
    public long getItemId(int arg0) {
        return 0;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        ViewHolder holder = null;
        if (convertView == null) {
            holder = new ViewHolder(); 
            convertView = mInflater.inflate(mResource, parent, false );
            holder.from = (TextView)convertView.findViewById(mTo[RES_FROM_ID]);
            holder.subject = (TextView)convertView.findViewById(mTo[RES_SUBJECT_ID]);
            holder.date = (TextView)convertView.findViewById(mTo[RES_DATE_ID]);
            holder.badge = (QuickContactBadge)convertView.findViewById(mTo[RES_BADGE_ID]);
            convertView.setTag(holder);
        }
        else {
            holder = (ViewHolder)convertView.getTag();
        }
         
        holder.from.setText((String)mData.get(position).get(mFrom[RES_FROM_ID]));
        holder.subject.setText((String)mData.get(position).get(mFrom[RES_SUBJECT_ID]));
        holder.date.setText((String)mData.get(position).get(mFrom[RES_DATE_ID]));
        holder.badge.setMode(QuickContact.MODE_SMALL);
        holder.badge.setImageDrawable((Drawable)mData.get(position).get(mFrom[RES_AVATAR_ID]));
        holder.badge.assignContactFromPhone((String)mData.get(position).get(mFrom[RES_BADGE_ID]), true);  
         
        return convertView;
    }
}
