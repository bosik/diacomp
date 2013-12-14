package org.bosik.compensation.persistence.dao.local.utils;

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

public class DiaryContentProvider extends ContentProvider
{
	// private static final String TAG = DiaryContentProvider.class.getSimpleName();

	// Core
	private MyDBHelper				openHelper;
	private static final UriMatcher	sURIMatcher;

	// Database
	private static final String		DATABASE_NAME				= "Comp.db";
	private static final int		DATABASE_VERSION			= 1;
	private static final String		SCHEME						= "content://";
	private static final String		AUTH						= "diacomp.provider";

	// ======================================= Diary table =======================================

	private static final String		TABLE_DIARY					= "diary";
	private static final String		COLUMN_DIARY_ID				= "_ID";
	public static final String		COLUMN_DIARY_DATE			= "Date";
	public static final String		COLUMN_DIARY_TIMESTAMP		= "TimeStamp";
	public static final String		COLUMN_DIARY_VERSION		= "Version";
	public static final String		COLUMN_DIARY_PAGE			= "Page";

	public static final String		CONTENT_DIARY_STRING		= SCHEME + AUTH + "/" + TABLE_DIARY + "/";
	public static final Uri			CONTENT_DIARY_URI			= Uri.parse(CONTENT_DIARY_STRING);

	private static final int		CODE_DIARY					= 1;

	// ===================================== Foodbase table =====================================

	private static final String		TABLE_FOODBASE				= "foodbase";
	public static final String		COLUMN_FOODBASE_GUID		= "GUID";
	public static final String		COLUMN_FOODBASE_TIMESTAMP	= "TimeStamp";
	public static final String		COLUMN_FOODBASE_VERSION		= "Version";
	public static final String		COLUMN_FOODBASE_DATA		= "Data";

	public static final String		CONTENT_FOODBASE_STRING		= SCHEME + AUTH + "/" + TABLE_FOODBASE + "/";
	public static final Uri			CONTENT_FOODBASE_URI		= Uri.parse(CONTENT_FOODBASE_STRING);

	private static final int		CODE_FOODBASE				= 2;

	// ===================================== Dishbase table =====================================

	private static final String		TABLE_DISHBASE				= "dishbase";
	public static final String		COLUMN_DISHBASE_GUID		= "GUID";
	public static final String		COLUMN_DISHBASE_TIMESTAMP	= "TimeStamp";
	public static final String		COLUMN_DISHBASE_VERSION		= "Version";
	public static final String		COLUMN_DISHBASE_DATA		= "Data";

	public static final String		CONTENT_DISHBASE_STRING		= SCHEME + AUTH + "/" + TABLE_DISHBASE + "/";
	public static final Uri			CONTENT_DISHBASE_URI		= Uri.parse(CONTENT_DISHBASE_STRING);

	private static final int		CODE_DISHBASE				= 3;

	static
	{
		sURIMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sURIMatcher.addURI(AUTH, TABLE_DIARY, CODE_DIARY);
		sURIMatcher.addURI(AUTH, TABLE_FOODBASE, CODE_FOODBASE);
		sURIMatcher.addURI(AUTH, TABLE_DISHBASE, CODE_DISHBASE);
		// sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/", CODE_DIARY_ITEM);
		// sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/date", CODE_DIARY_ITEM_DATE);
		// sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/stamp", CODE_DIARY_ITEM_TIMESTAMP);
		// sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/version", CODE_DIARY_ITEM_VERSION);
		// sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/page", CODE_DIARY_ITEM_PAGE);
	}

	private static final class MyDBHelper extends SQLiteOpenHelper
	{
		public MyDBHelper(Context context)
		{
			super(context, DATABASE_NAME, null, DATABASE_VERSION);
		}

		/*
		 * Creates the data repository. This is called when the provider attempts to open the
		 * repository and SQLite reports that it doesn't exist.
		 */
		@Override
		public void onCreate(SQLiteDatabase db)
		{
			// db.execSQL("DROP TABLE " + TABLE_DIARY);

			// @formatter:off
			
			// diary table			
			final String SQL_CREATE_DIARY = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s, %s)",
					TABLE_DIARY,
					COLUMN_DIARY_ID + " INTEGER PRIMARY KEY AUTOINCREMENT",
					COLUMN_DIARY_DATE + " TEXT NOT NULL UNIQUE",
					COLUMN_DIARY_TIMESTAMP + " TEXT",
					COLUMN_DIARY_VERSION + " INTEGER",
					COLUMN_DIARY_PAGE + " BLOB");
			db.execSQL(SQL_CREATE_DIARY);
			
			// foodbase table			
			final String SQL_CREATE_FOODBASE = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s)",
					TABLE_FOODBASE,
					COLUMN_FOODBASE_GUID + " TEXT PRIMARY KEY",
					COLUMN_FOODBASE_TIMESTAMP + " TEXT",
					COLUMN_FOODBASE_VERSION + " INTEGER",
					COLUMN_FOODBASE_DATA + " TEXT");
			db.execSQL(SQL_CREATE_FOODBASE);
			
			// dishbase table
			final String SQL_CREATE_DISHBASE = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s)",
					TABLE_DISHBASE,
					COLUMN_DISHBASE_GUID + " TEXT PRIMARY KEY",
					COLUMN_DISHBASE_TIMESTAMP + " TEXT",
					COLUMN_DISHBASE_VERSION + " INTEGER",
					COLUMN_DISHBASE_DATA + " TEXT");
			db.execSQL(SQL_CREATE_DISHBASE);
			
			// @formatter:on
		}

		@Override
		public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
		{
		}
	}

	private static final class UnknownUriException extends IllegalArgumentException
	{
		private static final long	serialVersionUID	= 1L;

		public UnknownUriException(Uri uri)
		{
			super("Unknown URI: " + uri);
		}
	}

	@Override
	public String getType(Uri uri)
	{
		// TODO: заглушка
		return "vnd.android.cursor.item.place_type_here";
	}

	@Override
	public boolean onCreate()
	{
		openHelper = new MyDBHelper(getContext());
		return true;
	}

	private void checkValues(ContentValues values, String key, String errorMsg)
	{
		if (!values.containsKey(key))
		{
			throw new IllegalArgumentException(errorMsg);
		}
	}

	private void checkNull(Object x, String errorMsg)
	{
		if (null == x)
		{
			throw new NullPointerException(errorMsg);
		}
	}

	// =================================== CRUD ===================================

	@Override
	public Uri insert(final Uri uri, final ContentValues values)
	{
		checkNull(uri, "URI is null");
		checkNull(values, "Values are null");

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
			{
				checkValues(values, COLUMN_DIARY_DATE, "No date specified");
				checkValues(values, COLUMN_DIARY_TIMESTAMP, "No timestamp specified");
				checkValues(values, COLUMN_DIARY_VERSION, "No version specified");
				checkValues(values, COLUMN_DIARY_PAGE, "No page specified");

				SQLiteDatabase db = openHelper.getWritableDatabase();
				long rowId = db.insert(TABLE_DIARY, COLUMN_DIARY_PAGE, values);

				if (rowId > 0)
				{
					Uri resultUri = ContentUris.withAppendedId(CONTENT_DIARY_URI, rowId);
					getContext().getContentResolver().notifyChange(resultUri, null);
					return resultUri;
				}
				else
				{
					throw new SQLException("Failed to insert row into " + uri);
				}
			}

			default:
				throw new UnknownUriException(uri);
		}
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder)
	{
		SQLiteQueryBuilder qb = new SQLiteQueryBuilder();

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
				SQLiteDatabase db = openHelper.getReadableDatabase();
				qb.setTables(TABLE_DIARY);
				Cursor cursor = qb.query(db, projection, selection, selectionArgs, null, null, sortOrder);
				cursor.setNotificationUri(getContext().getContentResolver(), uri);
				return cursor;

			default:
				throw new UnknownUriException(uri);
		}
	}

	@Override
	public int update(Uri uri, ContentValues values, String where, String[] whereArgs)
	{
		SQLiteDatabase db = openHelper.getWritableDatabase();
		int affectedCount;

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
				affectedCount = db.update(TABLE_DIARY, values, where, whereArgs);
				getContext().getContentResolver().notifyChange(uri, null);
				return affectedCount;

			default:
				throw new UnknownUriException(uri);
		}
	}

	@Override
	public int delete(final Uri uri, String where, String[] whereArgs)
	{
		// Deleting is disabled right now
		// TODO: mark as removed instead
		return 0;

		// SQLiteDatabase db = openHelper.getWritableDatabase();
		// int count;
		//
		// switch (sURIMatcher.match(uri))
		// {
		// case CODE_DIARY:
		// // от греха подальше...
		// if ((where != null) && (!where.equals("")))
		// {
		// // Log.d(TAG,"delete(): URI is correct (whole diary, checking WHERE clause...)");
		// count = db.delete(TABLE_DIARY, where, whereArgs);
		// // Log.d(TAG,"delete(): done");
		// }
		// else
		// {
		// throw new
		// IllegalArgumentException("Empty WHERE clause, this will destroy all database; denied");
		// }
		// break;
		//
		// // case CODE_DIARY_ITEM:
		// // String finalWhere = COLUMN_DIARY_ID + " = " + uri.getLastPathSegment();
		// //
		// // if ((where != null) && (!where.equals("")))
		// // {
		// // finalWhere = finalWhere + " AND " + where;
		// // }
		// //
		// // count = db.delete(TABLE_DIARY, // The database table name.
		// // finalWhere, // The final WHERE clause
		// // whereArgs // The incoming where clause values.
		// // );
		// // break;
		//
		// default:
		// throw new UnknownUriException(uri);
		// }
		//
		// getContext().getContentResolver().notifyChange(uri, null);
		//
		// return count;
	}
}