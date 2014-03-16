package org.bosik.diacomp.android.backend.common;

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
	public static final String		COLUMN_DIARY_GUID			= "_GUID";
	public static final String		COLUMN_DIARY_TIMESTAMP		= "_TimeStamp";
	public static final String		COLUMN_DIARY_VERSION		= "_Version";
	public static final String		COLUMN_DIARY_DELETED		= "_Deleted";
	public static final String		COLUMN_DIARY_CONTENT		= "_Content";
	public static final String		COLUMN_DIARY_TIMECACHE		= "_TimeCache";

	public static final String		CONTENT_DIARY_STRING		= SCHEME + AUTH + "/" + TABLE_DIARY + "/";
	public static final Uri			CONTENT_DIARY_URI			= Uri.parse(CONTENT_DIARY_STRING);

	private static final int		CODE_DIARY					= 1;

	// ===================================== Foodbase table =====================================

	private static final String		TABLE_FOODBASE				= "foodbase";
	public static final String		COLUMN_FOODBASE_GUID		= "GUID";
	public static final String		COLUMN_FOODBASE_TIMESTAMP	= "TimeStamp";
	public static final String		COLUMN_FOODBASE_VERSION		= "Version";
	public static final String		COLUMN_FOODBASE_DELETED		= "Deleted";
	public static final String		COLUMN_FOODBASE_DATA		= "Data";
	public static final String		COLUMN_FOODBASE_NAMECACHE	= "NameCache";

	public static final String		CONTENT_FOODBASE_STRING		= SCHEME + AUTH + "/" + TABLE_FOODBASE + "/";
	public static final Uri			CONTENT_FOODBASE_URI		= Uri.parse(CONTENT_FOODBASE_STRING);

	private static final int		CODE_FOODBASE				= 2;

	// ===================================== Dishbase table =====================================

	private static final String		TABLE_DISHBASE				= "dishbase";
	public static final String		COLUMN_DISHBASE_GUID		= "GUID";
	public static final String		COLUMN_DISHBASE_TIMESTAMP	= "TimeStamp";
	public static final String		COLUMN_DISHBASE_VERSION		= "Version";
	public static final String		COLUMN_DISHBASE_DELETED		= "Deleted";
	public static final String		COLUMN_DISHBASE_DATA		= "Data";

	public static final String		CONTENT_DISHBASE_STRING		= SCHEME + AUTH + "/" + TABLE_DISHBASE + "/";
	public static final Uri			CONTENT_DISHBASE_URI		= Uri.parse(CONTENT_DISHBASE_STRING);

	private static final int		CODE_DISHBASE				= 3;

	// ==================================================================================================

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
			db.execSQL("DROP TABLE IF EXISTS " + TABLE_DIARY);
			db.execSQL("DROP TABLE IF EXISTS " + TABLE_FOODBASE);
			db.execSQL("DROP TABLE IF EXISTS " + TABLE_DISHBASE);

			// @formatter:off

			// diary table
			final String SQL_CREATE_DIARY = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s, %s, %s)",
					TABLE_DIARY,
					COLUMN_DIARY_GUID + " CHAR(32) NOT NULL",
					COLUMN_DIARY_TIMESTAMP + " TEXT NOT NULL",
					COLUMN_DIARY_VERSION + " INTEGER",
					COLUMN_DIARY_DELETED + " INTEGER",
					COLUMN_DIARY_CONTENT + " BLOB",
					COLUMN_DIARY_TIMECACHE + " TEXT NOT NULL"
					);
			db.execSQL(SQL_CREATE_DIARY);

			// foodbase table
			final String SQL_CREATE_FOODBASE = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s, %s, %s)",
					TABLE_FOODBASE,
					COLUMN_FOODBASE_GUID + " TEXT PRIMARY KEY",
					COLUMN_FOODBASE_TIMESTAMP + " TEXT",
					COLUMN_FOODBASE_VERSION + " INTEGER",
					COLUMN_FOODBASE_DELETED + " INTEGER", // 0 or 1
					COLUMN_FOODBASE_NAMECACHE + " TEXT",
					COLUMN_FOODBASE_DATA + " TEXT");
			db.execSQL(SQL_CREATE_FOODBASE);

			// dishbase table
			final String SQL_CREATE_DISHBASE = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s, %s)",
					TABLE_DISHBASE,
					COLUMN_DISHBASE_GUID + " TEXT PRIMARY KEY",
					COLUMN_DISHBASE_TIMESTAMP + " TEXT",
					COLUMN_DISHBASE_VERSION + " INTEGER",
					COLUMN_DISHBASE_DELETED + " INTEGER", // 0 or 1
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

	private static void checkValues(ContentValues values, String key)
	{
		if (!values.containsKey(key))
		{
			throw new IllegalArgumentException(String.format("Field '%s' must be specified", key));
		}
	}

	private static void checkNull(Object x, String errorMsg)
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

		SQLiteDatabase db = openHelper.getWritableDatabase();
		Uri resultUri;

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
			{
				checkValues(values, COLUMN_DIARY_GUID);
				checkValues(values, COLUMN_DIARY_TIMESTAMP);
				checkValues(values, COLUMN_DIARY_VERSION);
				checkValues(values, COLUMN_DIARY_DELETED);
				checkValues(values, COLUMN_DIARY_CONTENT);
				checkValues(values, COLUMN_DIARY_TIMECACHE);

				long rowId = db.insert(TABLE_DIARY, null, values);

				if (rowId > 0)
				{
					resultUri = ContentUris.withAppendedId(CONTENT_DIARY_URI, rowId);
				}
				else
				{
					throw new SQLException("Failed to insert row into " + uri);
				}
				break;
			}

			case CODE_FOODBASE:
			{
				checkValues(values, COLUMN_FOODBASE_GUID);
				checkValues(values, COLUMN_FOODBASE_TIMESTAMP);
				checkValues(values, COLUMN_FOODBASE_VERSION);
				checkValues(values, COLUMN_FOODBASE_DATA);

				long rowId = db.insert(TABLE_FOODBASE, null, values);

				if (rowId > 0)
				{
					resultUri = ContentUris.withAppendedId(CONTENT_FOODBASE_URI, rowId);
				}
				else
				{
					throw new SQLException("Failed to insert row into " + uri);
				}
				break;
			}

			default:
			{
				throw new UnknownUriException(uri);
			}
		}

		getContext().getContentResolver().notifyChange(resultUri, null);
		return resultUri;
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder)
	{
		SQLiteDatabase db = openHelper.getReadableDatabase();
		SQLiteQueryBuilder qb = new SQLiteQueryBuilder();

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
			{
				qb.setTables(TABLE_DIARY);
				break;
			}
			case CODE_FOODBASE:
			{
				qb.setTables(TABLE_FOODBASE);
				break;
			}
			case CODE_DISHBASE:
			{
				qb.setTables(TABLE_DISHBASE);
				break;
			}

			default:
			{
				throw new UnknownUriException(uri);
			}
		}

		Cursor cursor = qb.query(db, projection, selection, selectionArgs, null, null, sortOrder);
		cursor.setNotificationUri(getContext().getContentResolver(), uri);
		return cursor;
	}

	@Override
	public int update(Uri uri, ContentValues values, String where, String[] whereArgs)
	{
		SQLiteDatabase db = openHelper.getWritableDatabase();
		int affectedCount;

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
			{
				affectedCount = db.update(TABLE_DIARY, values, where, whereArgs);
				break;
			}
			case CODE_FOODBASE:
			{
				affectedCount = db.update(TABLE_FOODBASE, values, where, whereArgs);
				break;
			}
			case CODE_DISHBASE:
			{
				affectedCount = db.update(TABLE_DISHBASE, values, where, whereArgs);
				break;
			}
			default:
			{
				throw new UnknownUriException(uri);
			}
		}

		getContext().getContentResolver().notifyChange(uri, null);
		return affectedCount;
	}

	@Override
	public int delete(final Uri uri, String where, String[] whereArgs)
	{
		/**
		 * TODO: This actually removes data from DB. Service should just mark rows deleted instead
		 * (using update method)
		 */

		SQLiteDatabase db = openHelper.getWritableDatabase();
		int count;

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
			{
				count = db.delete(TABLE_DIARY, where, whereArgs);
				break;
			}
			case CODE_FOODBASE:
			{
				count = db.delete(TABLE_FOODBASE, where, whereArgs);
				break;
			}
			case CODE_DISHBASE:
			{
				count = db.delete(TABLE_FOODBASE, where, whereArgs);
				break;
			}
			default:
			{
				throw new UnknownUriException(uri);
			}
		}

		getContext().getContentResolver().notifyChange(uri, null);
		return count;
	}
}