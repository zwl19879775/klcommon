package com.kl.android.BlackListProvider;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.content.UriMatcher;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.database.sqlite.SQLiteQueryBuilder;
import android.net.Uri;
import android.text.TextUtils;
import android.util.Log;

public class BlackListProvider extends ContentProvider {
    private static final String TAG = "BlackListProvider";
    private static final String DATABASE_NAME = "black_list.db";
    private static final int DATABASE_VERSION = 2;
    private static final String BLACKLIST_TABLE_NAME = "black_list";	
    private static final UriMatcher sUriMatcher;
    private static final int BLACKLIST = 1;
    private static final int BLACKLIST_ID = 2;
    private static final int BLOCKLOG = 3;
    private static final int BLOCKLOG_ID = 4;
    private static final String BLOCKLOG_TABLE_NAME = "block_log";	
    static {
        sUriMatcher = new UriMatcher(UriMatcher.NO_MATCH);
        sUriMatcher.addURI(BlackList.AUTHORITY, "blacklist", BLACKLIST);
        sUriMatcher.addURI(BlackList.AUTHORITY, "blacklist/#", BLACKLIST_ID);
        sUriMatcher.addURI(BlackList.AUTHORITY, "blocklog", BLOCKLOG);
        sUriMatcher.addURI(BlackList.AUTHORITY, "blocklog/#", BLOCKLOG_ID);
    }
    private static class DatabaseHelper extends SQLiteOpenHelper {

        DatabaseHelper(Context context) {
            super(context, DATABASE_NAME, null, DATABASE_VERSION);
        }

        @Override
        public void onCreate(SQLiteDatabase db) {
            db.execSQL("CREATE TABLE " + BLACKLIST_TABLE_NAME + " ("
                    + BlackList._ID + " INTEGER PRIMARY KEY,"
                    + BlackList.ADDRESS + " TEXT,"
                    + BlackList.TYPE + " INTEGER,"
                    + BlackList.DATE + " INTEGER,"
                    + BlackList.LATEST_LOG + " TEXT,"
                    + BlackList.LOG_COUNT + " INTEGER"
                    + ");");
            db.execSQL("CREATE TABLE " + BLOCKLOG_TABLE_NAME + " ("
                    + BlackList.BlockLog._ID + " INTEGER PRIMARY KEY,"
                    + BlackList.BlockLog.TYPE+ " INTEGER,"
                    + BlackList.BlockLog.DESC+ " TEXT,"
                    + BlackList.BlockLog.BLACK_ID + " INTEGER,"
                    + BlackList.BlockLog.DATE + " INTEGER"
                    + ");");
        }

        @Override
        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
            Log.w(TAG, "Upgrading database from version " + oldVersion + " to "
                    + newVersion + ", which will destroy all old data");
            db.execSQL("DROP TABLE IF EXISTS " + BLACKLIST_TABLE_NAME);
            db.execSQL("DROP TABLE IF EXISTS " + BLOCKLOG_TABLE_NAME);
            onCreate(db);
        }
    }

    private DatabaseHelper mOpenHelper;
    
    @Override
    public boolean onCreate() {
        mOpenHelper = new DatabaseHelper(getContext());
        return true;
    }

    @Override
    public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs,
            String sortOrder) {
        SQLiteQueryBuilder qb = new SQLiteQueryBuilder();
        qb.setTables(BLACKLIST_TABLE_NAME);

        switch (sUriMatcher.match(uri)) {
        case BLACKLIST:
        	break;
        case BLACKLIST_ID:
            qb.appendWhere(BlackList._ID + "=" + uri.getPathSegments().get(1));
        	break;
        case BLOCKLOG:
        	qb.setTables(BLOCKLOG_TABLE_NAME);
        	break;
        case BLOCKLOG_ID:
        	qb.setTables(BLOCKLOG_TABLE_NAME);
            qb.appendWhere(BlackList.BlockLog._ID + "=" + uri.getPathSegments().get(1));
        	break;
        default:
            throw new IllegalArgumentException("Unknown URI " + uri);
        }
        
        String orderBy;
        if(TextUtils.isEmpty(sortOrder)) {
            orderBy = BlackList.DEFAULT_SORT_ORDER;
        } 
        else {
            orderBy = sortOrder;
        }

        SQLiteDatabase db = mOpenHelper.getReadableDatabase();
        Cursor c = qb.query(db, projection, selection, selectionArgs, null, null, orderBy);

        c.setNotificationUri(getContext().getContentResolver(), uri);
        return c; 
    }
    
    @Override
    public String getType(Uri uri) {
        switch(sUriMatcher.match(uri)) {
        case BLACKLIST:
        	return BlackList.CONTENT_TYPE;
        case BLACKLIST_ID:
            return BlackList.CONTENT_ITEM_TYPE;
        case BLOCKLOG:
        	return BlackList.BlockLog.CONTENT_TYPE;
        case BLOCKLOG_ID:
            return BlackList.BlockLog.CONTENT_ITEM_TYPE;
        default:
            throw new IllegalArgumentException("Unknown URI " + uri);
        }
    }

    @Override
    public Uri insert(Uri uri, ContentValues values) {
        if (sUriMatcher.match(uri) != BLACKLIST &&
        	sUriMatcher.match(uri) != BLOCKLOG) {
            throw new IllegalArgumentException("Unknown URI " + uri);
        }
        if(values == null) {
        	throw new IllegalArgumentException("Must specify content values.");
        }
        int id = sUriMatcher.match(uri);
        if(id == BLACKLIST) {
        	Long now = Long.valueOf(System.currentTimeMillis());
        	if (values.containsKey(BlackList.DATE) == false) {
        		values.put(BlackList.DATE, now);
        	}
        	if (values.containsKey(BlackList.ADDRESS) == false) {
        		throw new IllegalArgumentException("Must provide ADDRESS argument.");
        	}
        	if (values.containsKey(BlackList.TYPE) == false) {
        		values.put(BlackList.TYPE, BlackList.TYPE_DEFAULT);
        	}
        	SQLiteDatabase db = mOpenHelper.getWritableDatabase();
        	long rowId = db.insert(BLACKLIST_TABLE_NAME, BlackList.ADDRESS, values);
        	if (rowId > 0) {
        		Uri noteUri = ContentUris.withAppendedId(BlackList.CONTENT_URI, rowId);
        		getContext().getContentResolver().notifyChange(noteUri, null);
        		return noteUri;
        	}

        	throw new SQLException("Failed to insert row into " + uri);
        }
        else {
        	Long now = Long.valueOf(System.currentTimeMillis());
        	if (values.containsKey(BlackList.BlockLog.DATE) == false) {
        		values.put(BlackList.BlockLog.DATE, now);
        	}
        	if (values.containsKey(BlackList.BlockLog.BLACK_ID) == false) {
        		throw new IllegalArgumentException("Must provide BLACK_ID argument.");
        	}
        	if (values.containsKey(BlackList.BlockLog.TYPE) == false) {
        		throw new IllegalArgumentException("Must provide TYPE argument.");
        	}	
        	SQLiteDatabase db = mOpenHelper.getWritableDatabase();
        	long rowId = db.insert(BLOCKLOG_TABLE_NAME, BlackList.BlockLog.BLACK_ID, values);
        	if (rowId > 0) {
        		// update black list log snippet.
        		updateBlackLogSnippet(values.getAsLong(BlackList.BlockLog.BLACK_ID),
        				values.getAsString(BlackList.BlockLog.DESC));
        		Uri noteUri = ContentUris.withAppendedId(BlackList.CONTENT_URI, rowId);
        		getContext().getContentResolver().notifyChange(noteUri, null);
        		getContext().getContentResolver().notifyChange(BlackList.BlockLog.CONTENT_URI, null);
        		return noteUri;
        	}
        	throw new SQLException("Failed to insert row into " + uri);
        }
    }

    // Currently, it does not support Log update/delete, only insert.
    private void updateBlackLogSnippet(long blackID, String log) {
    	SQLiteDatabase db = mOpenHelper.getReadableDatabase();
    	Cursor cursor = db.query(BLOCKLOG_TABLE_NAME, new String[] { "count(*)" },
    			BlackList.BlockLog.BLACK_ID + "=?",
    			new String[] { String.valueOf(blackID) }, null, null, null);
    	if(!cursor.moveToFirst()) {
    		return;
    	}
    	long cnt = cursor.getLong(0);
    	db = mOpenHelper.getWritableDatabase();
    	ContentValues val = new ContentValues();
    	val.put(BlackList.LOG_COUNT, cnt);
    	val.put(BlackList.LATEST_LOG, log);
    	db.update(BLACKLIST_TABLE_NAME, val, BlackList._ID + "=?", new String[] { String.valueOf(blackID) });
    }
    
    @Override
    public int delete(Uri uri, String where, String[] whereArgs) {
        SQLiteDatabase db = mOpenHelper.getWritableDatabase();
        int count;
        switch (sUriMatcher.match(uri)) {
        case BLACKLIST:
            count = db.delete(BLACKLIST_TABLE_NAME, where, whereArgs);
            break;
        case BLACKLIST_ID:
            String noteId = uri.getPathSegments().get(1);
            count = db.delete(BLACKLIST_TABLE_NAME, BlackList._ID + "=" + noteId
                    + (!TextUtils.isEmpty(where) ? " AND (" + where + ')' : ""), whereArgs);
            break;
        case BLOCKLOG:
            count = db.delete(BLOCKLOG_TABLE_NAME, where, whereArgs);
            break;
        case BLOCKLOG_ID: 
            noteId = uri.getPathSegments().get(1);
            count = db.delete(BLOCKLOG_TABLE_NAME, BlackList.BlockLog._ID + "=" + noteId
                    + (!TextUtils.isEmpty(where) ? " AND (" + where + ')' : ""), whereArgs);
            break;
        default:
            throw new IllegalArgumentException("Unknown URI " + uri);
        }

        getContext().getContentResolver().notifyChange(uri, null);
        return count;
    }

    @Override
    public int update(Uri uri, ContentValues values, String where, String[] whereArgs) {
        SQLiteDatabase db = mOpenHelper.getWritableDatabase();
        int count;
        switch (sUriMatcher.match(uri)) {
        case BLACKLIST:
            count = db.update(BLACKLIST_TABLE_NAME, values, where, whereArgs);
            break;
        case BLACKLIST_ID:
            String noteId = uri.getPathSegments().get(1);
            count = db.update(BLACKLIST_TABLE_NAME, values, BlackList._ID + "=" + noteId
                    + (!TextUtils.isEmpty(where) ? " AND (" + where + ')' : ""), whereArgs);
            break;
        case BLOCKLOG:
            count = db.update(BLOCKLOG_TABLE_NAME, values, where, whereArgs);
            break;
        case BLOCKLOG_ID:
            noteId = uri.getPathSegments().get(1);
            count = db.update(BLOCKLOG_TABLE_NAME, values, BlackList.BlockLog._ID + "=" + noteId
                    + (!TextUtils.isEmpty(where) ? " AND (" + where + ')' : ""), whereArgs);
            break;
        default:
            throw new IllegalArgumentException("Unknown URI " + uri);
        }

        getContext().getContentResolver().notifyChange(uri, null);
        return count;
    }
}
