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
	private static final String		DATABASE_NAME					= "Comp.db";
	private static final int		DATABASE_VERSION				= 1;
	private static final String		SCHEME							= "content://";
	public static final String		AUTHORITY						= "diacomp.provider";

	// ======================================= Diary table =======================================

	private static final String		TABLE_DIARY						= "diary";
	public static final String		COLUMN_DIARY_GUID				= "_GUID";
	public static final String		COLUMN_DIARY_TIMESTAMP			= "_TimeStamp";
	public static final String		COLUMN_DIARY_HASH				= "_Hash";
	public static final String		COLUMN_DIARY_VERSION			= "_Version";
	public static final String		COLUMN_DIARY_DELETED			= "_Deleted";
	public static final String		COLUMN_DIARY_CONTENT			= "_Content";
	public static final String		COLUMN_DIARY_TIMECACHE			= "_TimeCache";

	public static final String		CONTENT_DIARY_STRING			= SCHEME + AUTHORITY + "/" + TABLE_DIARY + "/";
	public static final Uri			CONTENT_DIARY_URI				= Uri.parse(CONTENT_DIARY_STRING);

	private static final int		CODE_DIARY						= 1;

	// ==================================== Diary hash table ====================================

	private static final String		TABLE_DIARY_HASH				= "diary_hash";
	public static final String		COLUMN_DIARY_HASH_GUID			= "_GUID";
	public static final String		COLUMN_DIARY_HASH_HASH			= "_Hash";

	public static final String		CONTENT_DIARY_HASH_STRING		= SCHEME + AUTHORITY + "/" + TABLE_DIARY_HASH + "/";
	public static final Uri			CONTENT_DIARY_HASH_URI			= Uri.parse(CONTENT_DIARY_HASH_STRING);

	private static final int		CODE_DIARY_HASH					= 10;

	// ===================================== Foodbase table =====================================

	private static final String		TABLE_FOODBASE					= "foodbase";
	public static final String		COLUMN_FOODBASE_GUID			= "GUID";
	public static final String		COLUMN_FOODBASE_TIMESTAMP		= "TimeStamp";
	public static final String		COLUMN_FOODBASE_HASH			= "Hash";
	public static final String		COLUMN_FOODBASE_VERSION			= "Version";
	public static final String		COLUMN_FOODBASE_DELETED			= "Deleted";
	public static final String		COLUMN_FOODBASE_DATA			= "Data";
	public static final String		COLUMN_FOODBASE_NAMECACHE		= "NameCache";

	public static final String		CONTENT_FOODBASE_STRING			= SCHEME + AUTHORITY + "/" + TABLE_FOODBASE + "/";
	public static final Uri			CONTENT_FOODBASE_URI			= Uri.parse(CONTENT_FOODBASE_STRING);

	private static final int		CODE_FOODBASE					= 2;

	// ==================================== Food hash table ====================================

	private static final String		TABLE_FOODBASE_HASH				= "foodbase_hash";
	public static final String		COLUMN_FOODBASE_HASH_GUID		= "_GUID";
	public static final String		COLUMN_FOODBASE_HASH_HASH		= "_Hash";

	public static final String		CONTENT_FOODBASE_HASH_STRING	= SCHEME + AUTHORITY + "/" + TABLE_FOODBASE_HASH
																			+ "/";
	public static final Uri			CONTENT_FOODBASE_HASH_URI		= Uri.parse(CONTENT_FOODBASE_HASH_STRING);

	private static final int		CODE_FOODBASE_HASH				= 20;

	// ===================================== Dishbase table =====================================

	private static final String		TABLE_DISHBASE					= "dishbase";
	public static final String		COLUMN_DISHBASE_GUID			= "GUID";
	public static final String		COLUMN_DISHBASE_TIMESTAMP		= "TimeStamp";
	public static final String		COLUMN_DISHBASE_HASH			= "Hash";
	public static final String		COLUMN_DISHBASE_VERSION			= "Version";
	public static final String		COLUMN_DISHBASE_DELETED			= "Deleted";
	public static final String		COLUMN_DISHBASE_DATA			= "Data";
	public static final String		COLUMN_DISHBASE_NAMECACHE		= "NameCache";

	public static final String		CONTENT_DISHBASE_STRING			= SCHEME + AUTHORITY + "/" + TABLE_DISHBASE + "/";
	public static final Uri			CONTENT_DISHBASE_URI			= Uri.parse(CONTENT_DISHBASE_STRING);

	private static final int		CODE_DISHBASE					= 3;

	// ==================================== Dish hash table ====================================

	private static final String		TABLE_DISHBASE_HASH				= "dish_hash";
	public static final String		COLUMN_DISHBASE_HASH_GUID		= "_GUID";
	public static final String		COLUMN_DISHBASE_HASH_HASH		= "_Hash";

	public static final String		CONTENT_DISHBASE_HASH_STRING	= SCHEME + AUTHORITY + "/" + TABLE_DISHBASE_HASH
																			+ "/";
	public static final Uri			CONTENT_DISHBASE_HASH_URI		= Uri.parse(CONTENT_DISHBASE_HASH_STRING);

	private static final int		CODE_DISHBASE_HASH				= 30;

	// ===================================== Tags table =====================================

	private static final String		TABLE_TAG						= "tag";
	public static final String		COLUMN_TAG_GUID					= "GUID";
	public static final String		COLUMN_TAG_TAG					= "Tag";

	public static final String		CONTENT_TAG_STRING				= SCHEME + AUTHORITY + "/" + TABLE_TAG + "/";
	public static final Uri			CONTENT_TAG_URI					= Uri.parse(CONTENT_TAG_STRING);

	private static final int		CODE_TAG						= 4;

	// ==================================================================================================

	static
	{
		sURIMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sURIMatcher.addURI(AUTHORITY, TABLE_DIARY, CODE_DIARY);
		sURIMatcher.addURI(AUTHORITY, TABLE_DIARY_HASH, CODE_DIARY_HASH);
		sURIMatcher.addURI(AUTHORITY, TABLE_FOODBASE, CODE_FOODBASE);
		sURIMatcher.addURI(AUTHORITY, TABLE_FOODBASE_HASH, CODE_FOODBASE_HASH);
		sURIMatcher.addURI(AUTHORITY, TABLE_DISHBASE, CODE_DISHBASE);
		sURIMatcher.addURI(AUTHORITY, TABLE_DISHBASE_HASH, CODE_DISHBASE_HASH);
		sURIMatcher.addURI(AUTHORITY, TABLE_TAG, CODE_TAG);
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
			// FIXME: THIS ERASES ALL DATA

			// db.execSQL("DROP TABLE IF EXISTS " + TABLE_DIARY);
			// db.execSQL("DROP TABLE IF EXISTS " + TABLE_FOODBASE);
			// db.execSQL("DROP TABLE IF EXISTS " + TABLE_DISHBASE);
			// db.execSQL("DROP TABLE IF EXISTS " + TABLE_TAG);

			// @formatter:off

			// diary table
			final String SQL_CREATE_DIARY = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s,%s,  %s, %s, %s, %s)",
				TABLE_DIARY,
				COLUMN_DIARY_GUID + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_DIARY_TIMESTAMP + " TEXT NOT NULL",
				COLUMN_DIARY_HASH + " TEXT NOT NULL",
				COLUMN_DIARY_VERSION + " INTEGER",
				COLUMN_DIARY_DELETED + " INTEGER",
				COLUMN_DIARY_CONTENT + " BLOB",
				COLUMN_DIARY_TIMECACHE + " TEXT NOT NULL"
			);
			db.execSQL(SQL_CREATE_DIARY);
			
			// diary hash table
			final String SQL_CREATE_DIARY_HASH = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s)",
				TABLE_DIARY_HASH,
				COLUMN_DIARY_HASH_GUID + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_DIARY_HASH_HASH + " TEXT NOT NULL"
			);
			db.execSQL(SQL_CREATE_DIARY_HASH);

			// foodbase table
			final String SQL_CREATE_FOODBASE = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s, %s, %s, %s)",
				TABLE_FOODBASE,
				COLUMN_FOODBASE_GUID + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_FOODBASE_TIMESTAMP + " TEXT",
				COLUMN_FOODBASE_HASH + " TEXT NOT NULL",
				COLUMN_FOODBASE_VERSION + " INTEGER",
				COLUMN_FOODBASE_DELETED + " INTEGER", // 0 or 1
				COLUMN_FOODBASE_NAMECACHE + " TEXT",
				COLUMN_FOODBASE_DATA + " TEXT"
			);
			db.execSQL(SQL_CREATE_FOODBASE);
			
			// foodbase hash table
			final String SQL_CREATE_FOODBASE_HASH = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s)",
				TABLE_FOODBASE_HASH,
				COLUMN_FOODBASE_HASH_GUID + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_FOODBASE_HASH_HASH + " TEXT NOT NULL"
			);
			db.execSQL(SQL_CREATE_FOODBASE_HASH);

			// dishbase table
			final String SQL_CREATE_DISHBASE = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s, %s, %s, %s)",
				TABLE_DISHBASE,
				COLUMN_DISHBASE_GUID + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_DISHBASE_TIMESTAMP + " TEXT",
				COLUMN_DISHBASE_HASH + " TEXT NOT NULL",
				COLUMN_DISHBASE_VERSION + " INTEGER",
				COLUMN_DISHBASE_DELETED + " INTEGER", // 0 or 1
				COLUMN_DISHBASE_NAMECACHE + " TEXT",
				COLUMN_DISHBASE_DATA + " TEXT"
			);
			db.execSQL(SQL_CREATE_DISHBASE);
			
			// foodbase hash table
			final String SQL_CREATE_DISHBASE_HASH = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s)",
				TABLE_DISHBASE_HASH,
				COLUMN_DISHBASE_HASH_GUID + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_DISHBASE_HASH_HASH + " TEXT NOT NULL"
			);
			db.execSQL(SQL_CREATE_DISHBASE_HASH);
			
			// tag table
			final String SQL_CREATE_TAG = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s)",
				TABLE_TAG,
				COLUMN_TAG_GUID + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_TAG_TAG + " INTEGER NOT NULL"
			);
			db.execSQL(SQL_CREATE_TAG);

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
		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
			{
				return "org.bosik.diacomp.diary";
			}
			case CODE_DIARY_HASH:
			{
				return "org.bosik.diacomp.diary.hash";
			}
			case CODE_FOODBASE:
			{
				return "org.bosik.diacomp.food";
			}
			case CODE_FOODBASE_HASH:
			{
				return "org.bosik.diacomp.food.hash";
			}
			case CODE_DISHBASE:
			{
				return "org.bosik.diacomp.dish";
			}
			case CODE_DISHBASE_HASH:
			{
				return "org.bosik.diacomp.dish.hash";
			}
			case CODE_TAG:
			{
				return "org.bosik.diacomp.tag";
			}
			default:
			{
				return "UNKNOWN";
			}
		}
	}

	@Override
	public boolean onCreate()
	{
		openHelper = new MyDBHelper(getContext());
		return true;
	}

	private static void assertDefined(ContentValues values, String key)
	{
		if (!values.containsKey(key))
		{
			throw new IllegalArgumentException(String.format("Field '%s' must be specified", key));
		}
	}

	private static void assertNotNull(Object x, String errorMsg)
	{
		if (null == x)
		{
			throw new IllegalArgumentException(errorMsg);
		}
	}

	// =================================== CRUD ===================================

	@Override
	public Uri insert(final Uri uri, final ContentValues values)
	{
		assertNotNull(uri, "URI is null");
		assertNotNull(values, "Values are null");

		SQLiteDatabase db = openHelper.getWritableDatabase();
		Uri resultUri;

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
			{
				assertDefined(values, COLUMN_DIARY_GUID);
				assertDefined(values, COLUMN_DIARY_TIMESTAMP);
				assertDefined(values, COLUMN_DIARY_HASH);
				assertDefined(values, COLUMN_DIARY_VERSION);
				assertDefined(values, COLUMN_DIARY_DELETED);
				assertDefined(values, COLUMN_DIARY_CONTENT);
				assertDefined(values, COLUMN_DIARY_TIMECACHE);

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

			case CODE_DIARY_HASH:
			{
				assertDefined(values, COLUMN_DIARY_HASH_GUID);
				assertDefined(values, COLUMN_DIARY_HASH_HASH);

				long rowId = db.insert(TABLE_DIARY_HASH, null, values);

				if (rowId > 0)
				{
					resultUri = ContentUris.withAppendedId(CONTENT_DIARY_HASH_URI, rowId);
				}
				else
				{
					throw new SQLException("Failed to insert row into " + uri);
				}
				break;
			}

			case CODE_FOODBASE:
			{
				assertDefined(values, COLUMN_FOODBASE_GUID);
				assertDefined(values, COLUMN_FOODBASE_TIMESTAMP);
				assertDefined(values, COLUMN_FOODBASE_HASH);
				assertDefined(values, COLUMN_FOODBASE_VERSION);
				assertDefined(values, COLUMN_FOODBASE_DATA);

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

			case CODE_FOODBASE_HASH:
			{
				assertDefined(values, COLUMN_FOODBASE_HASH_GUID);
				assertDefined(values, COLUMN_FOODBASE_HASH_HASH);

				long rowId = db.insert(TABLE_FOODBASE_HASH, null, values);

				if (rowId > 0)
				{
					resultUri = ContentUris.withAppendedId(CONTENT_FOODBASE_HASH_URI, rowId);
				}
				else
				{
					throw new SQLException("Failed to insert row into " + uri);
				}
				break;
			}

			case CODE_DISHBASE:
			{
				assertDefined(values, COLUMN_DISHBASE_GUID);
				assertDefined(values, COLUMN_DISHBASE_TIMESTAMP);
				assertDefined(values, COLUMN_DISHBASE_HASH);
				assertDefined(values, COLUMN_DISHBASE_VERSION);
				assertDefined(values, COLUMN_DISHBASE_DATA);

				long rowId = db.insert(TABLE_DISHBASE, null, values);

				if (rowId > 0)
				{
					resultUri = ContentUris.withAppendedId(CONTENT_DISHBASE_URI, rowId);
				}
				else
				{
					throw new SQLException("Failed to insert row into " + uri);
				}
				break;
			}

			case CODE_DISHBASE_HASH:
			{
				assertDefined(values, COLUMN_DISHBASE_HASH_GUID);
				assertDefined(values, COLUMN_DISHBASE_HASH_HASH);

				long rowId = db.insert(TABLE_DISHBASE_HASH, null, values);

				if (rowId > 0)
				{
					resultUri = ContentUris.withAppendedId(CONTENT_DISHBASE_HASH_URI, rowId);
				}
				else
				{
					throw new SQLException("Failed to insert row into " + uri);
				}
				break;
			}

			case CODE_TAG:
			{
				assertDefined(values, COLUMN_TAG_GUID);
				assertDefined(values, COLUMN_TAG_TAG);

				long rowId = db.insert(TABLE_TAG, null, values);

				if (rowId > 0)
				{
					resultUri = ContentUris.withAppendedId(CONTENT_TAG_URI, rowId);
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
			case CODE_DIARY_HASH:
			{
				qb.setTables(TABLE_DIARY_HASH);
				break;
			}
			case CODE_FOODBASE:
			{
				qb.setTables(TABLE_FOODBASE);
				break;
			}
			case CODE_FOODBASE_HASH:
			{
				qb.setTables(TABLE_FOODBASE_HASH);
				break;
			}
			case CODE_DISHBASE:
			{
				qb.setTables(TABLE_DISHBASE);
				break;
			}
			case CODE_DISHBASE_HASH:
			{
				qb.setTables(TABLE_DISHBASE_HASH);
				break;
			}
			case CODE_TAG:
			{
				qb.setTables(TABLE_TAG);
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
			case CODE_DIARY_HASH:
			{
				affectedCount = db.update(TABLE_DIARY_HASH, values, where, whereArgs);
				break;
			}
			case CODE_FOODBASE:
			{
				affectedCount = db.update(TABLE_FOODBASE, values, where, whereArgs);
				break;
			}
			case CODE_FOODBASE_HASH:
			{
				affectedCount = db.update(TABLE_FOODBASE_HASH, values, where, whereArgs);
				break;
			}
			case CODE_DISHBASE:
			{
				affectedCount = db.update(TABLE_DISHBASE, values, where, whereArgs);
				break;
			}
			case CODE_DISHBASE_HASH:
			{
				affectedCount = db.update(TABLE_DISHBASE_HASH, values, where, whereArgs);
				break;
			}
			case CODE_TAG:
			{
				affectedCount = db.update(TABLE_TAG, values, where, whereArgs);
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
		 * NOTE: This actually removes data from DB. Service should just mark rows deleted instead
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
			case CODE_DIARY_HASH:
			{
				count = db.delete(TABLE_DIARY_HASH, where, whereArgs);
				break;
			}
			case CODE_FOODBASE:
			{
				count = db.delete(TABLE_FOODBASE, where, whereArgs);
				break;
			}
			case CODE_FOODBASE_HASH:
			{
				count = db.delete(TABLE_FOODBASE_HASH, where, whereArgs);
				break;
			}
			case CODE_DISHBASE:
			{
				count = db.delete(TABLE_DISHBASE, where, whereArgs);
				break;
			}
			case CODE_DISHBASE_HASH:
			{
				count = db.delete(TABLE_DISHBASE_HASH, where, whereArgs);
				break;
			}
			case CODE_TAG:
			{
				count = db.delete(TABLE_TAG, where, whereArgs);
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