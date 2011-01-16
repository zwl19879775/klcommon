package com.kl.BlackList;

import java.util.HashMap;
import java.util.Map;
import android.view.View;
import android.widget.CheckBox;

public abstract class CheckBoxBinder implements CustomListAdapter.ViewBinder {
	private Map<Integer, Boolean> mStatus;
	private int mResId;

	public CheckBoxBinder(int id) {
		mResId = id;
		mStatus = new HashMap<Integer, Boolean>();
	}
	
	public void setViewValue(View view, int pos) {
		CheckBox box = (CheckBox) view.findViewById(mResId);
		box.setChecked(isChecked(pos));
	}
	
	public void toggle(View parent, int pos) {
		CheckBox box = (CheckBox) parent.findViewById(mResId);
		box.toggle();
		mStatus.put(pos, box.isChecked());
	}
	
	public boolean isChecked(int pos) {
		if(!mStatus.containsKey(pos)) return false;
		return mStatus.get(pos);
	}
}
