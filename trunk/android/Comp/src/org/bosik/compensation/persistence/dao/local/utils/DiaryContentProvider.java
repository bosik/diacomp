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

	// база SQL
	private static final String		TABLE_DIARY					= "diary";
	private static final String		COLUMN_DIARY_ID				= "_ID";
	public static final String		COLUMN_DIARY_DATE			= "Date";
	public static final String		COLUMN_DIARY_TIMESTAMP		= "TimeStamp";
	public static final String		COLUMN_DIARY_VERSION		= "Version";
	public static final String		COLUMN_DIARY_PAGE			= "Page";

	// константы провайдера (для доступа извне)
	private static final String		SCHEME						= "content://";
	private static final String		AUTH						= "diacomp.provider";
	// public static final Uri SHORT_URI = Uri.parse(AUTH + "/" + TABLE_DIARY + "/");
	public static final String		CONTENT_DIARY_STRING		= SCHEME + AUTH + "/" + TABLE_DIARY + "/";
	public static final Uri			CONTENT_DIARY_URI			= Uri.parse(CONTENT_DIARY_STRING);

	// Database params
	private static final String		DATABASE_NAME				= "Diary.db";
	private static final int		DATABASE_VERSION			= 1;

	// константы для распознавателя URI
	private static final int		CODE_DIARY					= 1;
	// private static final int CODE_DIARY_ITEM = 2;
	private static final int		CODE_DIARY_ITEM_DATE		= 21;
	private static final int		CODE_DIARY_ITEM_TIMESTAMP	= 22;
	private static final int		CODE_DIARY_ITEM_VERSION		= 23;
	private static final int		CODE_DIARY_ITEM_PAGE		= 24;

	// вспомогательные объекты
	private SQLiteDatabase			db;
	private MyDBHelper				openHelper;
	private static final UriMatcher	sURIMatcher;

	static
	{
		// TODO: подставить константы
		sURIMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sURIMatcher.addURI(AUTH, TABLE_DIARY, CODE_DIARY);
		// sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/", CODE_DIARY_ITEM);
		sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/date", CODE_DIARY_ITEM_DATE);
		sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/stamp", CODE_DIARY_ITEM_TIMESTAMP);
		sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/version", CODE_DIARY_ITEM_VERSION);
		sURIMatcher.addURI(AUTH, TABLE_DIARY + "/#/page", CODE_DIARY_ITEM_PAGE);
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
			// @formatter:off
			// db.execSQL("DROP TABLE " + TABLE_DIARY);
			final String SQL_CREATE_DIARY = String.format("CREATE TABLE IF NOT EXISTS %s (%s, %s, %s, %s, %s)",
					TABLE_DIARY,
					COLUMN_DIARY_ID + " INTEGER PRIMARY KEY AUTOINCREMENT",
					COLUMN_DIARY_DATE + " TEXT NOT NULL UNIQUE",
					COLUMN_DIARY_TIMESTAMP + " TEXT",
					COLUMN_DIARY_VERSION + " INTEGER",
					COLUMN_DIARY_PAGE + " BLOB");
			db.execSQL(SQL_CREATE_DIARY);
			// @formatter:on
		}

		@Override
		public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
		{
		}
	}

	@Override
	public String getType(Uri uri)
	{
		// TODO: заглушка
		return "DiaryDataType";
	}

	@Override
	public boolean onCreate()
	{
		openHelper = new MyDBHelper(getContext());
		return true;
	}

	// =================================== CRUD ===================================

	@Override
	public Uri insert(final Uri uri, final ContentValues initialValues)
	{
		if (null == uri)
		{
			throw new NullPointerException("URI can't be null");
		}

		if (null == initialValues)
		{
			throw new NullPointerException("Values are null");
		}

		if (sURIMatcher.match(uri) != CODE_DIARY)
		{
			throw new IllegalArgumentException("URI must be " + AUTH + "/" + TABLE_DIARY);
		}

		final ContentValues values = new ContentValues(initialValues);

		if (!values.containsKey(COLUMN_DIARY_DATE))
		{
			throw new IllegalArgumentException("No date specified");
		}

		if (!values.containsKey(COLUMN_DIARY_TIMESTAMP))
		{
			throw new IllegalArgumentException("No timestamp specified");
		}

		if (!values.containsKey(COLUMN_DIARY_VERSION))
		{
			throw new IllegalArgumentException("No version specified");
		}

		if (!values.containsKey(COLUMN_DIARY_PAGE))
		{
			throw new IllegalArgumentException("No page specified");
		}

		db = openHelper.getWritableDatabase();

		long rowId = db.insert(TABLE_DIARY, // The table to insert into.
				COLUMN_DIARY_PAGE, // A hack, SQLite sets this column value to null if values is
									// empty.
				values // A map of column names, and the values to insert into the columns.
				);

		if (rowId > 0)
		{
			// Creates a URI with the note ID pattern and the new row ID appended to it.
			Uri resultUri = ContentUris.withAppendedId(CONTENT_DIARY_URI, rowId);

			// Notifies observers registered against this provider that the data changed.
			getContext().getContentResolver().notifyChange(resultUri, null);
			return resultUri;
		}
		else
		{
			throw new SQLException("Failed to insert row into " + uri);
		}
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder)
	{
		SQLiteQueryBuilder qb = new SQLiteQueryBuilder();

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
				qb.setTables(TABLE_DIARY);
				// String orderBy = COLUMN_DIARY_ID + " ASC";
				db = openHelper.getReadableDatabase();

				Cursor cursor = qb.query(db, // The database to query
						projection, // The columns to return from the query
						selection, // The columns for the where clause
						selectionArgs, // The values for the where clause
						null, // don't group the rows
						null, // don't filter by row groups
						sortOrder // The sort order
						);

				// Tells the Cursor what URI to watch, so it knows when its source data changes
				cursor.setNotificationUri(getContext().getContentResolver(), uri);
				return cursor;
			default:
				// If the CONTENT_DIARY_URI is not recognized, you should do some error handling
				// here.
				throw new IllegalArgumentException("Unknown URI " + uri);
		}
	}

	@Override
	public int update(Uri uri, ContentValues values, String where, String[] whereArgs)
	{
		SQLiteDatabase db = openHelper.getWritableDatabase();
		int count;

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
				count = db.update(TABLE_DIARY, // The database table name.
						values, // A map of column names and new values to use.
						where, // The where clause column names.
						whereArgs // The where clause column values to select on.
						);
				break;

			default:
				throw new IllegalArgumentException("Unknown URI " + uri);
		}

		/*
		 * Gets a handle to the content resolver object for the current context, and notifies it
		 * that the incoming URI changed. The object passes this along to the resolver framework,
		 * and observers that have registered themselves for the provider are notified.
		 */
		getContext().getContentResolver().notifyChange(uri, null);

		// Returns the number of rows updated.
		return count;
	}

	@Override
	public int delete(final Uri uri, String where, String[] whereArgs)
	{
		SQLiteDatabase db = openHelper.getWritableDatabase();
		int count;

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
				// от греха подальше...
				if ((where != null) && (!where.equals("")))
				{
					// Log.d(TAG,"delete(): URI is correct (whole diary, checking WHERE clause...)");
					count = db.delete(TABLE_DIARY, where, whereArgs);
					// Log.d(TAG,"delete(): done");
				}
				else
				{
					throw new IllegalArgumentException("Empty WHERE clause, this will destroy all database; denied");
				}
				break;

			// case CODE_DIARY_ITEM:
			// String finalWhere = COLUMN_DIARY_ID + " = " + uri.getLastPathSegment();
			//
			// if ((where != null) && (!where.equals("")))
			// {
			// finalWhere = finalWhere + " AND " + where;
			// }
			//
			// count = db.delete(TABLE_DIARY, // The database table name.
			// finalWhere, // The final WHERE clause
			// whereArgs // The incoming where clause values.
			// );
			// break;

			default:
				throw new IllegalArgumentException("URI is incorrect: " + uri);
		}

		/*
		 * Gets a handle to the content resolver object for the current context, and notifies it
		 * that the incoming URI changed. The object passes this along to the resolver framework,
		 * and observers that have registered themselves for the provider are notified.
		 */
		getContext().getContentResolver().notifyChange(uri, null);

		return count;
	}
}