package com.kl.BlackList;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;


public class CustomListAdapter extends BaseAdapter {
    private LayoutInflater mInflater;
    private ViewBinder mBinder;
    private int mResource;
    
    public interface ViewBinder {
    	void setViewValue(View view, int pos);
    	int valueSize();
    }
    
    public CustomListAdapter (Context context, ViewBinder binder, int resource) {
    	mInflater = LayoutInflater.from(context);
    	mBinder = binder;
    	mResource = resource;
    }
    
    @Override
    public int getCount() {
    	return mBinder.valueSize();
    }

    @Override
    public Object getItem(int position) {
    	return position;
    }

    @Override
    public long getItemId(int position) {
    	return position;
    }
    
    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
    	if(convertView == null) {
    		convertView = mInflater.inflate(mResource, parent, false);
    	}
    	mBinder.setViewValue(convertView, position);
    	return convertView;
    }
}
