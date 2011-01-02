/*
 * @author Kevin Lynx
 * @date 12.28.2010
 */
package com.kl.android.ReadSMS;

import java.util.ArrayList;
import java.util.List;


public class SelectedContact {
	private static SelectedContact mInst = new SelectedContact();
	/// The string is in this format : name<number>
	private List<String> mContacts = new ArrayList<String>();
	
	public static SelectedContact instance() {
		return mInst;
	}
	
	public void clear() {
		mContacts.clear();
	}
	
	public void add(String contact) {
		if( !mContacts.contains(contact) ) {
			Log.d("Add selected contact :" + contact);
			mContacts.add( contact );
		}
	}
	
	public String getAllDesc() {
		String desc = "";
		for(String s : mContacts) {
			desc += s;
			desc += ',';
		}
		return desc;
	}
	
	public String[] getNumbers() {
		String[] nums = new String[mContacts.size()];
		int i = 0;
		for(String s : mContacts) {
			nums[i++] = getNumber(s);
		}
		return nums;
	}
	
	/// format: name<number>, name<number>, number, <number>,
	public String[] getNumbersFromDesc(String desc) {
		ArrayList<String> nums = new ArrayList<String>();
		Log.i("Parse number string:" + desc);
		int p = 0;
		while(p < desc.length()) {
			int e = desc.indexOf(',', p);
			if(e == -1) e = desc.length();
			nums.add( getNumber( desc.substring(p, e) ));
			p = e + 1;
		}
		String[] ret = new String[nums.size()];
		return nums.toArray(ret);
	}
	
	/// Get string in <number> or number.
	public String getNumber(String desc) {
		int s = desc.indexOf('<');
		int es = s;
		if( es == -1 ) es = 0;
		int e = desc.indexOf('>', es);
		if( e == -1 ) e = desc.length();
		return desc.substring( s + 1, e ).trim();
	}
}
