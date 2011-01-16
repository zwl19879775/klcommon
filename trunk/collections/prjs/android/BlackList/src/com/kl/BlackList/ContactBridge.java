package com.kl.BlackList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ContactBridge {
	/// <Conversation id, contact>
	private static Map<Long, Contact.Item> mContacts = new HashMap<Long, Contact.Item>();
	
	public static Contact.Item bind(Conversation.Item conv, Contact.OnItemUpdate listener) {
		List<RecipientIdCache.Entry> entries = RecipientIdCache.getAddress(conv.recipientIds);
		if(entries == null) {
			Log.w(String.format("Get address failed on:%d", conv.id));
			return null;
		}
		// just care about the 1st one.
		Contact.Item contact = Contact.inst().startAsyncQuery(entries.get(0).number);
		if(listener != null) {
			contact.addListener(listener);
		}
		mContacts.put(conv.id, contact);
		return contact;
	}
	
	public static Contact.Item get(long convID) {
		return mContacts.get(convID);
	}
}
