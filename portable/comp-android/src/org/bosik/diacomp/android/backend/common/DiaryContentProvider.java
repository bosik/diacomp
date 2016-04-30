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

import java.util.ArrayList;
import java.util.List;

import org.bosik.diacomp.android.backend.common.db.Column;
import org.bosik.diacomp.android.backend.common.db.Table;
import org.bosik.diacomp.android.backend.common.db.tables.TableDiary;
import org.bosik.diacomp.android.backend.common.db.tables.TableDishbase;
import org.bosik.diacomp.android.backend.common.db.tables.TableFoodbase;
import org.bosik.diacomp.android.backend.common.db.tables.TableKoofs;
import org.bosik.diacomp.android.backend.common.db.tables.TableTags;

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
	private static final String			TAG							= DiaryContentProvider.class.getSimpleName();

	// Core
	private MyDBHelper					openHelper;
	public static final UriMatcher		sURIMatcher;

	// Database
	private static final String			DATABASE_NAME				= "Comp.db";
	private static final int			DATABASE_VERSION			= 1;
	private static final String			SCHEME						= "content://";
	public static final String			AUTHORITY					= "diacomp.provider";
	public static final Uri				CONTENT_BASE_URI			= Uri.parse(SCHEME + AUTHORITY + "/");

	// ===================================== Foodbase table =====================================

	public static final int				CODE_FOODBASE				= 2;

	// ===================================== Dishbase table =====================================

	public static final int				CODE_DISHBASE				= 3;

	// ===================================== Preferences table =====================================

	private static final String			TABLE_PREFERENCES			= "preferences";
	public static final String			COLUMN_PREFERENCES_KEY		= "Key";
	public static final String			COLUMN_PREFERENCES_VALUE	= "Value";
	public static final String			COLUMN_PREFERENCES_VERSION	= "Version";

	public static final Uri				CONTENT_PREFERENCES_URI		= Uri.parse(SCHEME + AUTHORITY + "/" + TABLE_PREFERENCES + "/");
	private static final int			CODE_PREFERENCES			= 5;

	// ==================================================================================================

	private static final List<Table>	tables						= new ArrayList<>();

	static
	{
		tables.add(new TableDiary()
		{
			@Override
			public int getCode()
			{
				return 1;
			}
		});
		tables.add(new TableFoodbase()
		{
			@Override
			public int getCode()
			{
				return CODE_FOODBASE;
			}
		});
		tables.add(new TableDishbase()
		{
			@Override
			public int getCode()
			{
				return CODE_DISHBASE;
			}
		});
		tables.add(new TableTags()
		{
			@Override
			public int getCode()
			{
				return 4;
			}
		});
		tables.add(new TableKoofs()
		{
			@Override
			public int getCode()
			{
				return 6;
			}
		});

		sURIMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sURIMatcher.addURI(AUTHORITY, TABLE_PREFERENCES, CODE_PREFERENCES);

		for (int i = 0 ; i < tables.size(); i++)
		{
			sURIMatcher.addURI(AUTHORITY, tables.get(i).getName(), tables.get(i).getCode());
		}
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
//			final String SQL_CREATE_DIARY = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s,%s,  %s, %s, %s, %s)",
//				TABLE_DIARY,
//				COLUMN_DIARY_GUID + " TEXT PRIMARY KEY NOT NULL",
//				COLUMN_DIARY_TIMESTAMP + " TEXT NOT NULL",
//				COLUMN_DIARY_HASH + " TEXT NOT NULL",
//				COLUMN_DIARY_VERSION + " INTEGER",
//				COLUMN_DIARY_DELETED + " INTEGER",
//				COLUMN_DIARY_CONTENT + " BLOB",
//				COLUMN_DIARY_TIMECACHE + " TEXT NOT NULL"
//			);
//			db.execSQL(SQL_CREATE_DIARY);

			// foodbase table
//			final String SQL_CREATE_FOODBASE = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s, %s, %s, %s)",
//				TABLE_FOODBASE,
//				TableFoodbase.COLUMN_ID + " TEXT PRIMARY KEY NOT NULL",
//				TableFoodbase.COLUMN_TIMESTAMP + " TEXT",
//				TableFoodbase.COLUMN_HASH + " TEXT NOT NULL",
//				TableFoodbase.COLUMN_VERSION + " INTEGER",
//				TableFoodbase.COLUMN_DELETED + " INTEGER", // 0 or 1
//				TableFoodbase.COLUMN_NAMECACHE + " TEXT",
//				TableFoodbase.COLUMN_DATA + " TEXT"
//			);
//			db.execSQL(SQL_CREATE_FOODBASE);
//
			// dishbase table
//			final String SQL_CREATE_DISHBASE = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s, %s, %s, %s)",
//				TABLE_DISHBASE,
//				TableDishbase.COLUMN_ID + " TEXT PRIMARY KEY NOT NULL",
//				TableDishbase.COLUMN_TIMESTAMP + " TEXT",
//				TableDishbase.COLUMN_HASH + " TEXT NOT NULL",
//				TableDishbase.COLUMN_VERSION + " INTEGER",
//				TableDishbase.COLUMN_DELETED + " INTEGER", // 0 or 1
//				TableDishbase.COLUMN_NAMECACHE + " TEXT",
//				TableDishbase.COLUMN_DATA + " TEXT"
//			);
//			db.execSQL(SQL_CREATE_DISHBASE);
			
			// tag table
//			final String SQL_CREATE_TAG = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s)",
//				TABLE_TAG,
//				TableTags.COLUMN_TAG_GUID + " TEXT PRIMARY KEY NOT NULL",
//				TableTags.COLUMN_TAG_TAG + " INTEGER NOT NULL"
//			);
//			db.execSQL(SQL_CREATE_TAG);
//			
			// preferences table
			final String SQL_CREATE_PREFERENCES = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s)",
				TABLE_PREFERENCES,
				COLUMN_PREFERENCES_KEY + " TEXT PRIMARY KEY NOT NULL",
				COLUMN_PREFERENCES_VALUE + " TEXT NOT NULL",
				COLUMN_PREFERENCES_VERSION + " INTEGER NOT NULL"
			);
			db.execSQL(SQL_CREATE_PREFERENCES);

			// @formatter:on

			for (Table table : tables)
			{
				StringBuilder s = new StringBuilder("CREATE TABLE IF NOT EXISTS ");
				s.append(table.getName()).append(" (");

				if (!table.getColumns().isEmpty())
				{
					for (Column c : table.getColumns())
					{
						s.append(c.getName()).append(" ");
						s.append(c.getType()).append(" ");
						if (c.isPrimary())
						{
							s.append("PRIMARY KEY ");
						}
						if (!c.isNullable())
						{
							s.append("NOT NULL ");
						}
						s.append(", ");
					}
					s.delete(s.length() - 2, s.length());
				}
				
				s.append(")");

				db.execSQL(s.toString());
			}
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
		int code = sURIMatcher.match(uri);

		for (Table table : tables)
		{
			if (code == table.getCode())
			{
				return table.getContentType();
			}
		}

		switch (code)
		{
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

	public static Uri buildUri(Table table)
	{
		return Uri.parse(SCHEME + AUTHORITY + "/" + table.getName() + "/");
	}
	
	// =================================== CRUD ===================================

	@Override
	public Uri insert(final Uri uri, final ContentValues values)
	{
		assertNotNull(uri, "URI is null");
		assertNotNull(values, "Values are null");

		SQLiteDatabase db = openHelper.getWritableDatabase();
		Uri resultUri;

		int code = sURIMatcher.match(uri);
		
		for (Table table : tables)
		{
			if (code == table.getCode())
			{
				for (Column column : table.getColumns())
				{
					if (!column.isNullable())
					{
						assertDefined(values, column.getName());
					}
				}

				long rowId = db.insert(table.getName(), null, values);

				if (rowId > 0)
				{
					resultUri = ContentUris.withAppendedId(buildUri(table), rowId);
					getContext().getContentResolver().notifyChange(resultUri, null);
					return resultUri;
				}
				else
				{
					throw new SQLException("Failed to insert row into " + uri);
				}
			}
		}
		
		switch (code)
		{
//			case CODE_DIARY:
//			{
//				assertDefined(values, COLUMN_DIARY_GUID);
//				assertDefined(values, COLUMN_DIARY_TIMESTAMP);
//				assertDefined(values, COLUMN_DIARY_HASH);
//				assertDefined(values, COLUMN_DIARY_VERSION);
//				assertDefined(values, COLUMN_DIARY_DELETED);
//				assertDefined(values, COLUMN_DIARY_CONTENT);
//				assertDefined(values, COLUMN_DIARY_TIMECACHE);
//
//				long rowId = db.insert(TABLE_DIARY, null, values);
//
//				if (rowId > 0)
//				{
//					resultUri = ContentUris.withAppendedId(CONTENT_DIARY_URI, rowId);
//				}
//				else
//				{
//					throw new SQLException("Failed to insert row into " + uri);
//				}
//				break;
//			}
//
//			case CODE_FOODBASE:
//			{
//				assertDefined(values, TableFoodbase.COLUMN_ID);
//				assertDefined(values, TableFoodbase.COLUMN_TIMESTAMP);
//				assertDefined(values, TableFoodbase.COLUMN_HASH);
//				assertDefined(values, TableFoodbase.COLUMN_VERSION);
//				assertDefined(values, TableFoodbase.COLUMN_DATA);
//
//				long rowId = db.insert(TABLE_FOODBASE, null, values);
//
//				if (rowId > 0)
//				{
//					resultUri = ContentUris.withAppendedId(TableFoodbase.CONTENT_URI, rowId);
//				}
//				else
//				{
//					throw new SQLException("Failed to insert row into " + uri);
//				}
//				break;
//			}
//
//			case CODE_DISHBASE:
//			{
//				assertDefined(values, TableDishbase.COLUMN_ID);
//				assertDefined(values, TableDishbase.COLUMN_TIMESTAMP);
//				assertDefined(values, TableDishbase.COLUMN_HASH);
//				assertDefined(values, TableDishbase.COLUMN_VERSION);
//				assertDefined(values, TableDishbase.COLUMN_DATA);
//
//				long rowId = db.insert(TABLE_DISHBASE, null, values);
//
//				if (rowId > 0)
//				{
//					resultUri = ContentUris.withAppendedId(TableDishbase.CONTENT_URI, rowId);
//				}
//				else
//				{
//					throw new SQLException("Failed to insert row into " + uri);
//				}
//				break;
//			}
//
//			case CODE_TAG:
//			{
//				assertDefined(values, TableTags.COLUMN_GUID);
//				assertDefined(values, TableTags.COLUMN_TAG);
//
//				long rowId = db.insert(TABLE_TAG, null, values);
//
//				if (rowId > 0)
//				{
//					resultUri = ContentUris.withAppendedId(TableTags.CONTENT_TAG_URI, rowId);
//				}
//				else
//				{
//					throw new SQLException("Failed to insert row into " + uri);
//				}
//				break;
//			}

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

		int code = sURIMatcher.match(uri);
		
		for (Table table : tables)
		{
			if (code == table.getCode())
			{
				qb.setTables(table.getName());
				Cursor cursor = qb.query(db, projection, selection, selectionArgs, null, null, sortOrder);
				cursor.setNotificationUri(getContext().getContentResolver(), uri);
				return cursor;
			}
		}
		
		switch (code)
		{
//			case CODE_DIARY:
//			{
//				qb.setTables(TABLE_DIARY);
//				break;
//			}
//			case CODE_FOODBASE:
//			{
//				qb.setTables(TABLE_FOODBASE);
//				break;
//			}
//			case CODE_DISHBASE:
//			{
//				qb.setTables(TABLE_DISHBASE);
//				break;
//			}
//			case CODE_TAG:
//			{
//				qb.setTables(TABLE_TAG);
//				break;
//			}

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

		int code = sURIMatcher.match(uri);
		
		for (Table table : tables)
		{
			if (code == table.getCode())
			{
				affectedCount = db.update(table.getName(), values, where, whereArgs);
				getContext().getContentResolver().notifyChange(uri, null);
				return affectedCount;
			}
		}
		
		switch (code)
		{
//			case CODE_DIARY:
//			{
//				affectedCount = db.update(TABLE_DIARY, values, where, whereArgs);
//				break;
//			}
//			case CODE_FOODBASE:
//			{
//				affectedCount = db.update(TABLE_FOODBASE, values, where, whereArgs);
//				break;
//			}
//			case CODE_DISHBASE:
//			{
//				affectedCount = db.update(TABLE_DISHBASE, values, where, whereArgs);
//				break;
//			}
//			case CODE_TAG:
//			{
//				affectedCount = db.update(TABLE_TAG, values, where, whereArgs);
//				break;
//			}
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

		int code = sURIMatcher.match(uri);
		
		for (Table table : tables)
		{
			if (code == table.getCode())
			{
				count = db.delete(table.getName(), where, whereArgs);
				getContext().getContentResolver().notifyChange(uri, null);
				return count;
			}
		}
		
		switch (code)
		{
//			case CODE_DIARY:
//			{
//				count = db.delete(TABLE_DIARY, where, whereArgs);
//				break;
//			}
//			case CODE_FOODBASE:
//			{
//				count = db.delete(TABLE_FOODBASE, where, whereArgs);
//				break;
//			}
//			case CODE_DISHBASE:
//			{
//				count = db.delete(TABLE_DISHBASE, where, whereArgs);
//				break;
//			}
//			case CODE_TAG:
//			{
//				count = db.delete(TABLE_TAG, where, whereArgs);
//				break;
//			}
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