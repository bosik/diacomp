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
import org.bosik.diacomp.android.backend.common.db.tables.TablePreferences;
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
	// Basic properties
	private static final String			DATABASE_NAME		= "Comp.db";
	private static final int			DATABASE_VERSION	= 2;
	private static final String			SCHEME				= "content://";
	public static final String			AUTHORITY			= "diacomp.provider";
	public static final Uri				CONTENT_BASE_URI	= Uri.parse(SCHEME + AUTHORITY + "/");

	// Core
	private MyDBHelper					openHelper;
	private static final UriMatcher		sURIMatcher;
	private static final List<Table>	tables;

	// ==================================================================================================

	static
	{
		tables = new ArrayList<>();

		tables.add(new TableDiary()
		{
			@Override
			public int getCode()
			{
				return 1;
			}
		});
		tables.add(new TableFoodbase());
		tables.add(new TableDishbase());
		tables.add(new TableTags()
		{
			@Override
			public int getCode()
			{
				return 4;
			}
		});
		tables.add(new TablePreferences()
		{
			@Override
			public int getCode()
			{
				return 5;
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

		for (int i = 0; i < tables.size(); i++)
		{
			sURIMatcher.addURI(AUTHORITY, tables.get(i).getName(), tables.get(i).getCode());
		}
	}

	public static final class MyDBHelper extends SQLiteOpenHelper
	{
		public MyDBHelper(Context context)
		{
			super(context, DATABASE_NAME, null, DATABASE_VERSION);
		}

		private String buildCreateTableStatement(Table table)
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
			return s.toString();
		}

		private String buildDropTableStatement(Table table)
		{
			return "DROP TABLE IF EXISTS " + table.getName();
		}

		@Override
		public void onCreate(SQLiteDatabase db)
		{
			// FIXME: THIS ERASES ALL DATA

			// db.execSQL("DROP TABLE IF EXISTS " + TABLE_DIARY);
			// db.execSQL("DROP TABLE IF EXISTS " + TABLE_FOODBASE);
			// db.execSQL("DROP TABLE IF EXISTS " + TABLE_DISHBASE);
			// db.execSQL("DROP TABLE IF EXISTS " + TABLE_TAG);

			for (Table table : tables)
			{
				String s = buildCreateTableStatement(table);
				db.execSQL(s);
			}
		}

		@Override
		public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
		{
			for (int version = oldVersion; version < newVersion; version++)
			{
				upgradeNext(db, version);
			}
		}

		private void upgradeNext(SQLiteDatabase db, int oldVersion)
		{
			switch (oldVersion)
			{
			case 1: // --> 2
			{
				TableKoofs tableKoofs = new TableKoofs();

				String s = buildDropTableStatement(tableKoofs);
				db.execSQL(s);

				s = buildCreateTableStatement(tableKoofs);
				db.execSQL(s);
				break;
			}
			}
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

	public static Table getTable(Uri uri)
	{
		int code = sURIMatcher.match(uri);
		for (Table table : tables)
		{
			if (code == table.getCode())
			{
				return table;
			}
		}

		return null;
	}

	@Override
	public String getType(Uri uri)
	{
		Table table = getTable(uri);

		if (table != null)
		{
			return table.getContentType();
		}
		else
		{
			return "UNKNOWN";
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

		Table table = getTable(uri);

		if (table != null)
		{
			for (Column column : table.getColumns())
			{
				if (!column.isNullable())
				{
					assertDefined(values, column.getName());
				}
			}

			SQLiteDatabase db = openHelper.getWritableDatabase();
			long rowId = db.insert(table.getName(), null, values);

			if (rowId != -1)
			{
				Uri resultUri = ContentUris.withAppendedId(buildUri(table), rowId);
				getContext().getContentResolver().notifyChange(resultUri, null);
				return resultUri;
			}
			else
			{
				throw new SQLException("Failed to insert row into " + uri);
			}
		}
		else
		{
			throw new UnknownUriException(uri);
		}
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder)
	{
		Table table = getTable(uri);

		if (table != null)
		{
			SQLiteQueryBuilder qb = new SQLiteQueryBuilder();
			qb.setTables(table.getName());
			SQLiteDatabase db = openHelper.getReadableDatabase();
			Cursor cursor = qb.query(db, projection, selection, selectionArgs, null, null, sortOrder);
			cursor.setNotificationUri(getContext().getContentResolver(), uri);
			return cursor;
		}
		else
		{
			throw new UnknownUriException(uri);
		}
	}

	@Override
	public int update(Uri uri, ContentValues values, String where, String[] whereArgs)
	{
		Table table = getTable(uri);

		if (table != null)
		{
			SQLiteDatabase db = openHelper.getWritableDatabase();
			int affectedCount = db.update(table.getName(), values, where, whereArgs);
			getContext().getContentResolver().notifyChange(uri, null);
			return affectedCount;
		}
		else
		{
			throw new UnknownUriException(uri);
		}
	}

	@Override
	public int delete(final Uri uri, String where, String[] whereArgs)
	{
		/**
		 * NOTE: This method actually removes data from DB. Service should just mark rows deleted instead (using update method)
		 */
		Table table = getTable(uri);

		if (table != null)
		{
			SQLiteDatabase db = openHelper.getWritableDatabase();
			int count = db.delete(table.getName(), where, whereArgs);
			getContext().getContentResolver().notifyChange(uri, null);
			return count;
		}
		else
		{
			throw new UnknownUriException(uri);
		}
	}
}