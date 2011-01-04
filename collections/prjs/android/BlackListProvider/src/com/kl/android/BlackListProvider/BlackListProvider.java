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
    
    static {
        sUriMatcher = new UriMatcher(UriMatcher.NO_MATCH);
        sUriMatcher.addURI(BlackList.AUTHORITY, "blacklist", BLACKLIST);
        sUriMatcher.addURI(BlackList.AUTHORITY, "blacklist/#", BLACKLIST_ID);
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
                    + BlackList.DATE + " INTEGER"
                    + ");");
        }

        @Override
        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
            Log.w(TAG, "Upgrading database from version " + oldVersion + " to "
                    + newVersion + ", which will destroy all old data");
            db.execSQL("DROP TABLE IF EXISTS " + BLACKLIST_TABLE_NAME);
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
        default:
            throw new IllegalArgumentException("Unknown URI " + uri);
        }
    }

    @Override
    public Uri insert(Uri uri, ContentValues initialValues) {
        if (sUriMatcher.match(uri) != BLACKLIST) {
            throw new IllegalArgumentException("Unknown URI " + uri);
        }

        ContentValues values;
        if (initialValues != null) {
            values = new ContentValues(initialValues);
        } else {
            values = new ContentValues();
        }

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

        default:
            throw new IllegalArgumentException("Unknown URI " + uri);
        }

        getContext().getContentResolver().notifyChange(uri, null);
        return count;
    }
}
