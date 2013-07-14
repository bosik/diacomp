package org.bosik.compensation.persistence.providers;

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

public class DiaryProvider extends ContentProvider
{
	// отладочная печать
	@SuppressWarnings("unused")
	private static final String TAG = "DiaryProvider";

	// база SQL
	private static final String TABLE_NAME_DIARY = "diary";
	private static final String COLUMN_ID = "_ID";
	public static final String COLUMN_DATE = "Date";
	public static final String COLUMN_TIMESTAMP = "TimeStamp";
	public static final String COLUMN_VERSION = "Version";
	public static final String COLUMN_PAGE = "Page";

	// константы провайдера (для доступа извне)
	public static final String SCHEME = "content://";
	public static final String AUTH = "diacomp.provider";
	public static final Uri SHORT_URI = Uri.parse(AUTH + "/" + TABLE_NAME_DIARY + "/");
	public static final String CONTENT_STRING = SCHEME + AUTH + "/" + TABLE_NAME_DIARY + "/";
	public static final Uri CONTENT_URI = Uri.parse(CONTENT_STRING);

	// внутренние параметры
	private static final String DATABASE_NAME = "Diary.db";
	private static final int DATABASE_VERSION = 1;

	// константы для распознавателя URI
	private static final int CODE_DIARY = 1;
	private static final int CODE_DIARY_ITEM = 2;
	private static final int CODE_DIARY_ITEM_DATE = 21;
	private static final int CODE_DIARY_ITEM_TIMESTAMP = 22;
	private static final int CODE_DIARY_ITEM_VERSION = 23;
	private static final int CODE_DIARY_ITEM_PAGE = 24;

	// вспомогательные объекты
	private static final UriMatcher sURIMatcher;
	private MyDBHelper openHelper;
	private SQLiteDatabase db;

	static
	{
		// TODO: подставить константы
		sURIMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sURIMatcher.addURI(AUTH, TABLE_NAME_DIARY, CODE_DIARY);
		sURIMatcher.addURI(AUTH, TABLE_NAME_DIARY + "/#/", CODE_DIARY_ITEM);
		sURIMatcher.addURI(AUTH, TABLE_NAME_DIARY + "/#/date", CODE_DIARY_ITEM_DATE);
		sURIMatcher.addURI(AUTH, TABLE_NAME_DIARY + "/#/stamp", CODE_DIARY_ITEM_TIMESTAMP);
		sURIMatcher.addURI(AUTH, TABLE_NAME_DIARY + "/#/version", CODE_DIARY_ITEM_VERSION);
		sURIMatcher.addURI(AUTH, TABLE_NAME_DIARY + "/#/page", CODE_DIARY_ITEM_PAGE);
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
			// Log.i(TAG, "MyDBHelper.onCreate()");
			// db.execSQL("DROP TABLE " + TABLE_NAME_DIARY);
			db.execSQL("CREATE TABLE IF NOT EXISTS " + TABLE_NAME_DIARY + " (" + COLUMN_ID + " INTEGER PRIMARY KEY AUTOINCREMENT, " + COLUMN_DATE
					+ " TEXT NOT NULL UNIQUE, " + COLUMN_TIMESTAMP + " TEXT, " + COLUMN_VERSION + " INTEGER, " + COLUMN_PAGE + " BLOB)");
			// Log.d(TAG, "MyDBHelper.onCreate(): table '" + TABLE_NAME_DIARY + "' created OK");
		}

		public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
		{
		}
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
				if ((where != null) && (where != ""))
				{
					// Log.d(TAG,"delete(): URI is correct (whole diary, checking WHERE clause...)");
					count = db.delete(TABLE_NAME_DIARY, where, whereArgs);
					// Log.d(TAG,"delete(): done");
				} else
				{
					throw new IllegalArgumentException("Empty WHERE clause, this will destroy all database; denied");
				}
				break;

			case CODE_DIARY_ITEM:
				// Log.d(TAG,"delete(): URI is correct (item ID is specified I hope)");
				String finalWhere = COLUMN_ID + " = " + uri.getLastPathSegment();

				if ((where != null) && (where != ""))
				{
					finalWhere = finalWhere + " AND " + where;
				}

				count = db.delete(TABLE_NAME_DIARY, // The database table name.
						finalWhere, // The final WHERE clause
						whereArgs // The incoming where clause values.
						);
				// Log.d(TAG,"delete(): done");

				break;

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

	@Override
	public String getType(Uri uri)
	{
		// TODO: заглушка
		return "DiaryDataType";
	}

	@Override
	public Uri insert(final Uri uri, final ContentValues initialValues)
	{
		if (null == uri)
			throw new NullPointerException("URI can't be null");

		if (null == initialValues)
			throw new NullPointerException("Values are null");

		if (sURIMatcher.match(uri) != CODE_DIARY)
			throw new IllegalArgumentException("URI must be " + AUTH + "/" + TABLE_NAME_DIARY);

		final ContentValues values = new ContentValues(initialValues);

		if (!values.containsKey(COLUMN_DATE))
			throw new IllegalArgumentException("No date specified");

		if (!values.containsKey(COLUMN_TIMESTAMP))
			throw new IllegalArgumentException("No timestamp specified");

		if (!values.containsKey(COLUMN_VERSION))
			throw new IllegalArgumentException("No version specified");

		if (!values.containsKey(COLUMN_PAGE))
			throw new IllegalArgumentException("No page specified");

		db = openHelper.getWritableDatabase();

		long rowId = db.insert(TABLE_NAME_DIARY, // The table to insert into.
				COLUMN_PAGE, // A hack, SQLite sets this column value to null if values is empty.
				values // A map of column names, and the values to insert into the columns.
				);

		if (rowId > 0)
		{
			// Creates a URI with the note ID pattern and the new row ID appended to it.
			Uri resultUri = ContentUris.withAppendedId(CONTENT_URI, rowId);

			// Notifies observers registered against this provider that the data changed.
			getContext().getContentResolver().notifyChange(resultUri, null);
			return resultUri;
		} else
		{
			throw new SQLException("Failed to insert row into " + uri);
		}
	}

	@Override
	public boolean onCreate()
	{
		openHelper = new MyDBHelper(getContext());
		return true;
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder)
	{
		SQLiteQueryBuilder qb = new SQLiteQueryBuilder();
		qb.setTables(TABLE_NAME_DIARY);

		// TODO: определить допустимые URI, отрефакторить логику
		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
				break;
			default:
				// If the CONTENT_URI is not recognized, you should do some error handling here.
				throw new IllegalArgumentException("Unknown URI " + uri);
		}

		// String orderBy = COLUMN_ID + " ASC";
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
	}

	@Override
	public int update(Uri uri, ContentValues values, String where, String[] whereArgs)
	{
		SQLiteDatabase db = openHelper.getWritableDatabase();
		int count;

		switch (sURIMatcher.match(uri))
		{
			case CODE_DIARY:
				count = db.update(TABLE_NAME_DIARY, // The database table name.
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
}