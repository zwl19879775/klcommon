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
	public static final String ID = _ID;
	public static final String ADDRESS = "address";
	public static final String DATE = "date";
	public static final String TYPE = "type";
	public static final String LOG_COUNT = "log_count";
	public static final String LATEST_LOG = "latest_log";
	
	public static final int TYPE_MMS = 0x1;
	public static final int TYPE_PHONE = 0x2;
	public static final int TYPE_ALL = 0x4;
	public static final int TYPE_DEFAULT = TYPE_ALL;
	
	public final static class BlockLog implements BaseColumns {
		// This class cannot be instantiated
		private BlockLog() {}

		public static final Uri CONTENT_URI = Uri.parse("content://" + AUTHORITY + "/blocklog");
		public static final String CONTENT_TYPE = "vnd.android.cursor.dir/vnd." + AUTHORITY + "/blocklog";
		public static final String CONTENT_ITEM_TYPE = "vnd.android.cursor.item/vnd." + AUTHORITY + "/blocklog";
		public static final String ID = _ID;
		public static final String DESC = "desc";
		public static final String BLACK_ID = "black_id";
		public static final String DATE = "date";
		public static final String TYPE = "type";
		
		public static final int TYPE_MMS = 1;
		public static final int TYPE_CONTACT = 2;
	}
}

