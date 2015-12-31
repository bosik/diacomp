/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
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
	private static final String		TAG							= DiaryContentProvider.class.getSimpleName();

	// Core
	private MyDBHelper				openHelper;
	public static final UriMatcher	sURIMatcher;

	// Database
	private static final String		DATABASE_NAME				= "Comp.db";
	private static final int		DATABASE_VERSION			= 1;
	private static final String		SCHEME						= "content://";
	public static final String		AUTHORITY					= "diacomp.provider";
	public static final Uri			CONTENT_BASE_URI			= Uri.parse(SCHEME + AUTHORITY + "/");

	// ======================================= Diary table =======================================

	private static final String		TABLE_DIARY					= "diary";
	public static final String		COLUMN_DIARY_GUID			= "_GUID";
	public static final String		COLUMN_DIARY_TIMESTAMP		= "_TimeStamp";
	public static final String		COLUMN_DIARY_HASH			= "_Hash";
	public static final String		COLUMN_DIARY_VERSION		= "_Version";
	public static final String		COLUMN_DIARY_DELETED		= "_Deleted";
	public static final String		COLUMN_DIARY_CONTENT		= "_Content";
	public static final String		COLUMN_DIARY_TIMECACHE		= "_TimeCache";

	public static final String		CONTENT_DIARY_STRING		= SCHEME + AUTHORITY + "/" + TABLE_DIARY + "/";
	public static final Uri			CONTENT_DIARY_URI			= Uri.parse(CONTENT_DIARY_STRING);

	public static final int			CODE_DIARY					= 1;

	// ===================================== Foodbase table =====================================

	private static final String		TABLE_FOODBASE				= "foodbase";
	public static final String		COLUMN_FOODBASE_GUID		= "GUID";
	public static final String		COLUMN_FOODBASE_TIMESTAMP	= "TimeStamp";
	public static final String		COLUMN_FOODBASE_HASH		= "Hash";
	public static final String		COLUMN_FOODBASE_VERSION		= "Version";
	public static final String		COLUMN_FOODBASE_DELETED		= "Deleted";
	public static final String		COLUMN_FOODBASE_DATA		= "Data";
	public static final String		COLUMN_FOODBASE_NAMECACHE	= "NameCache";

	public static final String		CONTENT_FOODBASE_STRING		= SCHEME + AUTHORITY + "/" + TABLE_FOODBASE + "/";
	public static final Uri			CONTENT_FOODBASE_URI		= Uri.parse(CONTENT_FOODBASE_STRING);

	public static final int			CODE_FOODBASE				= 2;

	// ===================================== Dishbase table =====================================

	private static final String		TABLE_DISHBASE				= "dishbase";
	public static final String		COLUMN_DISHBASE_GUID		= "GUID";
	public static final String		COLUMN_DISHBASE_TIMESTAMP	= "TimeStamp";
	public static final String		COLUMN_DISHBASE_HASH		= "Hash";
	public static final String		COLUMN_DISHBASE_VERSION		= "Version";
	public static final String		COLUMN_DISHBASE_DELETED		= "Deleted";
	public static final String		COLUMN_DISHBASE_DATA		= "Data";
	public static final String		COLUMN_DISHBASE_NAMECACHE	= "NameCache";

	public static final String		CONTENT_DISHBASE_STRING		= SCHEME + AUTHORITY + "/" + TABLE_DISHBASE + "/";
	public static final Uri			CONTENT_DISHBASE_URI		= Uri.parse(CONTENT_DISHBASE_STRING);

	public static final int			CODE_DISHBASE				= 3;

	// ===================================== Tags table =====================================

	private static final String		TABLE_TAG					= "tag";
	public static final String		COLUMN_TAG_GUID				= "GUID";
	public static final String		COLUMN_TAG_TAG				= "Tag";

	public static final String		CONTENT_TAG_STRING			= SCHEME + AUTHORITY + "/" + TABLE_TAG + "/";
	public static final Uri			CONTENT_TAG_URI				= Uri.parse(CONTENT_TAG_STRING);

	public static final int			CODE_TAG					= 4;

	// ===================================== Preferences table =====================================

	private static final String		TABLE_PREFERENCES			= "preferences";
	public static final String		COLUMN_PREFERENCES_KEY		= "Key";
	public static final String		COLUMN_PREFERENCES_VALUE	= "Value";
	public static final String		COLUMN_PREFERENCES_VERSION	= "Version";

	public static final String		CONTENT_PREFERENCES_STRING	= SCHEME + AUTHORITY + "/" + TABLE_PREFERENCES + "/";
	public static final Uri			CONTENT_PREFERENCES_URI		= Uri.parse(CONTENT_PREFERENCES_STRING);

	public static final int			CODE_PREFERENCES			= 5;

	// ==================================================================================================

	static
	{
		sURIMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sURIMatcher.addURI(AUTHORITY, TABLE_DIARY, CODE_DIARY);
		sURIMatcher.addURI(AUTHORITY, TABLE_FOODBASE, CODE_FOODBASE);
		sURIMatcher.addURI(AUTHORITY, TABLE_DISHBASE, CODE_DISHBASE);
		sURIMatcher.addURI(AUTHORITY, TABLE_TAG, CODE_TAG);
		sURIMatcher.addURI(AUTHORITY, TABLE_PREFERENCES, CODE_PREFERENCES);
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
			
			// tag table
			final String SQL_CREATE_TAG = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s)",
				TABLE_TAG,
				COLUMN_TAG_GUID + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_TAG_TAG + " INTEGER NOT NULL"
			);
			db.execSQL(SQL_CREATE_TAG);
			
			// preferences table
			final String SQL_CREATE_PREFERENCES = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s)",
				TABLE_PREFERENCES,
				COLUMN_PREFERENCES_KEY + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_PREFERENCES_VALUE + " TEXT NOT NULL",
				COLUMN_PREFERENCES_VERSION + " INTEGER NOT NULL"
			);
			db.execSQL(SQL_CREATE_PREFERENCES);

			// @formatter:on
		}

		@Override
		public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
		{
		}
	}

	private static final class UnknownUriException extends IllegalArgumentException
	{
		private static final long serialVersionUID = 1L;

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
			case CODE_FOODBASE:
			{
				return "org.bosik.diacomp.food";
			}
			case CODE_DISHBASE:
			{
				return "org.bosik.diacomp.dish";
			}
			case CODE_TAG:
			{
				return "org.bosik.diacomp.tag";
			}
			case CODE_PREFERENCES:
			{
				return "org.bosik.diacomp.preferences";
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

			case CODE_PREFERENCES:
			{
				assertDefined(values, COLUMN_PREFERENCES_KEY);
				assertDefined(values, COLUMN_PREFERENCES_VALUE);
				assertDefined(values, COLUMN_PREFERENCES_VERSION);

				long rowId = db.insert(TABLE_PREFERENCES, null, values);

				if (rowId > 0)
				{
					resultUri = ContentUris.withAppendedId(CONTENT_PREFERENCES_URI, rowId);
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
			case CODE_TAG:
			{
				qb.setTables(TABLE_TAG);
				break;
			}

			case CODE_PREFERENCES:
			{
				qb.setTables(TABLE_PREFERENCES);
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
			case CODE_TAG:
			{
				affectedCount = db.update(TABLE_TAG, values, where, whereArgs);
				break;
			}
			case CODE_PREFERENCES:
			{
				affectedCount = db.update(TABLE_PREFERENCES, values, where, whereArgs);
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
			case CODE_FOODBASE:
			{
				count = db.delete(TABLE_FOODBASE, where, whereArgs);
				break;
			}
			case CODE_DISHBASE:
			{
				count = db.delete(TABLE_DISHBASE, where, whereArgs);
				break;
			}
			case CODE_TAG:
			{
				count = db.delete(TABLE_TAG, where, whereArgs);
				break;
			}
			case CODE_PREFERENCES:
			{
				count = db.delete(TABLE_PREFERENCES, where, whereArgs);
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