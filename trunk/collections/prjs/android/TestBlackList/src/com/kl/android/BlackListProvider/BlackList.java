package com.kl.android.BlackListProvider;

import android.net.Uri;
import android.provider.BaseColumns;

public final class BlackList implements BaseColumns {
	public static final String AUTHORITY = "com.kl.android.blacklistprovider";

	// This class cannot be instantiated
	private BlackList() {}

	public static final Uri CONTENT_URI = Uri.parse("content://" + AUTHORITY + "/blacklist");

	public static final String CONTENT_TYPE = "vnd.android.cursor.dir/vnd." + AUTHORITY + "/blacklist";
	
	public static final String CONTENT_ITEM_TYPE = "vnd.android.cursor.item/vnd." + AUTHORITY + "/blacklist";

	public static final String DEFAULT_SORT_ORDER = "date DESC";

	public static final String ID = "_id";
	public static final String ADDRESS = "address";
	public static final String DATE = "date";
	public static final String TYPE = "type";
	
	public static final int TYPE_MMS = 0x1;
	public static final int TYPE_PHONE = 0x2;
	public static final int TYPE_ALL = 0x4;
	public static final int TYPE_DEFAULT = TYPE_MMS;
}
