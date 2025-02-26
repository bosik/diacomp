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
package org.bosik.diacomp.android.backend.features.diary;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.util.JsonReader;
import android.util.Log;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider.MyDBHelper;
import org.bosik.diacomp.android.backend.common.db.CursorIterator;
import org.bosik.diacomp.android.backend.common.db.tables.TableDiary;
import org.bosik.diacomp.android.backend.common.stream.StreamReader;
import org.bosik.diacomp.android.backend.common.stream.versioned.DiaryRecordVersionedReader;
import org.bosik.diacomp.android.backend.features.quickImport.PlainDataImporter;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.services.transfer.Importable;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.Versioned;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

public class DiaryLocalService implements DiaryService, Importable
{
	private static final String TAG = DiaryLocalService.class.getSimpleName();

	private static final int MAX_READ_ITEMS = 500;

	/* ============================ FIELDS ============================ */

	private final Context                 context;
	private final ContentResolver         resolver;
	private final Parser<DiaryRecord>     parser     = new ParserDiaryRecord();
	private final Serializer<DiaryRecord> serializer = new SerializerAdapter<>(parser);

	/* ============================ CONSTRUCTOR ============================ */

	/**
	 * Constructor
	 *
	 * @param context
	 */
	public DiaryLocalService(Context context)
	{
		this.context = Objects.requireNonNull(context, "context is null");
		this.resolver = context.getContentResolver();
	}

	/* ============================ DB ============================ */

	private void insert(Versioned<DiaryRecord> record)
	{
		ContentValues newValues = new ContentValues();

		newValues.put(TableDiary.COLUMN_ID, record.getId());
		newValues.put(TableDiary.COLUMN_TIMESTAMP, Utils.formatTimeUTC(record.getTimeStamp()));
		newValues.put(TableDiary.COLUMN_HASH, record.getHash());
		newValues.put(TableDiary.COLUMN_VERSION, record.getVersion());
		newValues.put(TableDiary.COLUMN_DELETED, record.isDeleted());
		newValues.put(TableDiary.COLUMN_CONTENT, serializer.write(record.getData()));
		newValues.put(TableDiary.COLUMN_TIMECACHE, Utils.formatTimeUTC(record.getData().getTime()));

		resolver.insert(TableDiary.CONTENT_URI, newValues);
	}

	private void update(Versioned<DiaryRecord> record)
	{
		ContentValues newValues = new ContentValues();

		newValues.put(TableDiary.COLUMN_TIMESTAMP, Utils.formatTimeUTC(record.getTimeStamp()));
		newValues.put(TableDiary.COLUMN_HASH, record.getHash());
		newValues.put(TableDiary.COLUMN_VERSION, record.getVersion());
		newValues.put(TableDiary.COLUMN_DELETED, record.isDeleted());
		newValues.put(TableDiary.COLUMN_CONTENT, serializer.write(record.getData()));
		newValues.put(TableDiary.COLUMN_TIMECACHE, Utils.formatTimeUTC(record.getData().getTime()));

		String clause = TableDiary.COLUMN_ID + " = ?";
		String[] args = { record.getId() };
		resolver.update(TableDiary.CONTENT_URI, newValues, clause, args);
	}

	/* ============================ API ============================ */

	@Override
	public void add(Versioned<DiaryRecord> item) throws DuplicateException
	{
		if (verify(item))
		{
			if (!recordExists(item.getId()))
			{
				insert(item);
			}
			else
			{
				throw new DuplicateException(item.getId());
			}
		}
		else
		{
			Log.e(TAG, "Invalid record: " + item);
		}
	}

	@Override
	public int count(String prefix)
	{
		if (prefix == null)
		{
			throw new IllegalArgumentException("ID prefix is null");
		}

		String[] projection = new String[] { "count(*) AS count" };
		String clause = String.format("%s LIKE ?", TableDiary.COLUMN_ID);
		String[] clauseArgs = { prefix + "%" };

		try (Cursor cursor = resolver.query(TableDiary.CONTENT_URI, projection, clause, clauseArgs, null))
		{
			if (cursor != null)
			{
				cursor.moveToFirst();
				return cursor.getInt(0);
			}
			else
			{
				throw new IllegalArgumentException("Cursor is null");
			}
		}
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		Versioned<DiaryRecord> item = findById(id);

		if (item == null)
		{
			throw new NotFoundException(id);
		}
		if (item.isDeleted())
		{
			throw new AlreadyDeletedException(id);
		}

		item.setDeleted(true);
		item.modified();
		save(Collections.singletonList(item));
	}

	@Override
	public Versioned<DiaryRecord> findById(String id) throws CommonServiceException
	{
		// construct parameters
		String[] projection = null; // all
		String clause = TableDiary.COLUMN_ID + " = ?";
		String[] clauseArgs = { id };
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(TableDiary.CONTENT_URI, projection, clause, clauseArgs, sortOrder);

		List<Versioned<DiaryRecord>> recs = extractRecords(cursor);

		return recs.isEmpty() ?
				null :
				recs.get(0);
	}

	@Override
	public List<Versioned<DiaryRecord>> findByIdPrefix(String prefix)
	{
		// construct parameters
		String[] projection = null; // all
		String clause = String.format("%s LIKE ?", TableDiary.COLUMN_ID);
		String[] clauseArgs = { prefix + "%" };
		String sortOrder = TableDiary.COLUMN_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(TableDiary.CONTENT_URI, projection, clause, clauseArgs, sortOrder);

		return extractRecords(cursor, MAX_READ_ITEMS);
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date since) throws CommonServiceException
	{
		// construct parameters
		String[] projection = null;
		String clause = String.format("%s > ?", TableDiary.COLUMN_TIMESTAMP);
		String[] clauseArgs = { Utils.formatTimeUTC(since) };
		String sortOrder = TableDiary.COLUMN_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(TableDiary.CONTENT_URI, projection, clause, clauseArgs, sortOrder);
		return extractRecords(cursor, MAX_READ_ITEMS);
	}

	@Override
	public synchronized List<Versioned<DiaryRecord>> findPeriod(Date startTime, Date endTime, boolean includeRemoved)
			throws CommonServiceException
	{
		// long time = System.currentTimeMillis();

		if (startTime == null)
		{
			throw new IllegalArgumentException("startTime is null");
		}

		if (endTime == null)
		{
			throw new IllegalArgumentException("endTime is null");
		}

		// construct parameters
		String[] projection = null; // all

		String clause;
		String[] clauseArgs;

		if (includeRemoved)
		{
			clause = String.format("(%s >= ?) AND (%s < ?)", TableDiary.COLUMN_TIMECACHE, TableDiary.COLUMN_TIMECACHE);
			clauseArgs = new String[] { Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime) };
		}
		else
		{
			clause = String.format("(%s >= ?) AND (%s < ?) AND (%s = 0)", TableDiary.COLUMN_TIMECACHE, TableDiary.COLUMN_TIMECACHE,
					TableDiary.COLUMN_DELETED);
			clauseArgs = new String[] { Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime) };
		}

		String sortOrder = TableDiary.COLUMN_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(TableDiary.CONTENT_URI, projection, clause, clauseArgs, sortOrder);

		List<Versioned<DiaryRecord>> records = extractRecords(cursor);

		// detailed logging inside
		Verifier.verifyRecords(records, startTime, endTime);

		// time = System.currentTimeMillis() - time;
		// Log.d(TAG, String.format("#DBF %d items found between %s and %s in %d ms",
		// records.size(),
		// Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime), time));

		return records;
	}

	private boolean recordExists(String id)
	{
		final String[] select = { TableDiary.COLUMN_ID };
		final String where = String.format("%s = ?", TableDiary.COLUMN_ID);
		final String[] whereArgs = { id };
		final String sortOrder = null;

		try (Cursor cursor = resolver.query(TableDiary.CONTENT_URI, select, where, whereArgs, sortOrder))
		{
			return cursor != null && cursor.moveToFirst();
		}
	}

	@Override
	public void save(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		for (Versioned<DiaryRecord> record : records)
		{
			if (verify(record))
			{
				if (recordExists(record.getId()))
				{
					update(record);
				}
				else
				{
					insert(record);
				}
			}
			else
			{
				Log.e(TAG, "Invalid record: " + record);
			}
		}
	}

	private static boolean verify(Versioned<DiaryRecord> record)
	{
		if (record != null && record.getId() != null && record.getId().length() == ID_FULL_SIZE)
		{
			record.setId(record.getId().toLowerCase(Locale.US));
			return true;
		}
		else
		{
			return false;
		}
	}

	public void deletePermanently(String id) throws CommonServiceException
	{
		try
		{
			String clause = TableDiary.COLUMN_ID + " = ?";
			String[] args = { id };
			resolver.delete(TableDiary.CONTENT_URI, clause, args);
		}
		catch (Exception e)
		{
			throw new PersistenceException(e);
		}
	}

	private CursorIterator<String> getDataHashes(String prefix)
	{
		// constructing parameters
		final String[] select = { TableDiary.COLUMN_HASH };
		final boolean hasPrefix = prefix != null && prefix.length() > 0;
		final String selection = hasPrefix ?
				String.format("%s LIKE ?", TableDiary.COLUMN_ID) :
				null;
		final String[] selectionArgs = hasPrefix ?
				new String[] { prefix + "%" } :
				null;

		final Cursor cursor = resolver.query(TableDiary.CONTENT_URI, select, selection, selectionArgs, null);
		return cursor != null ?
				new CursorIterator<>(cursor, c -> cursor.getString(0)) :
				CursorIterator.empty();
	}

	@Override
	public String getHash(String prefix)
	{
		try (CursorIterator<String> hashes = getDataHashes(prefix))
		{
			return HashUtils.sum(hashes);
		}
	}

	@Override
	public Map<String, String> getHashChildren(String prefix)
	{
		final Map<String, String> map = new HashMap<>();

		for (int i = 0; i < 16; i++)
		{
			final String key = prefix + HashUtils.byteToChar(i);

			try (CursorIterator<String> hashes = getDataHashes(key))
			{
				map.put(key, HashUtils.sum(hashes));
			}
		}

		return map;
	}

	/* ======================= ROUTINES ========================= */

	/**
	 * Automatically closes cursor after read
	 *
	 * @param cursor
	 * @param limit
	 * @return
	 */
	private List<Versioned<DiaryRecord>> extractRecords(Cursor cursor, int limit)
	{
		if (cursor != null)
		{
			List<Versioned<DiaryRecord>> result = new ArrayList<>();

			while (cursor.moveToNext())
			{
				result.add(parseEntry(cursor));

				if (limit > 0 && result.size() > limit)
				{
					throw new TooManyItemsException("Too many items");
				}
			}

			cursor.close();

			return result;
		}
		else
		{
			throw new CommonServiceException(new IllegalArgumentException("Cursor is null"));
		}
	}

	private Versioned<DiaryRecord> parseEntry(Cursor cursor)
	{
		final int indexID = cursor.getColumnIndex(TableDiary.COLUMN_ID);
		final int indexTimestamp = cursor.getColumnIndex(TableDiary.COLUMN_TIMESTAMP);
		final int indexHash = cursor.getColumnIndex(TableDiary.COLUMN_HASH);
		final int indexVersion = cursor.getColumnIndex(TableDiary.COLUMN_VERSION);
		final int indexDeleted = cursor.getColumnIndex(TableDiary.COLUMN_DELETED);
		final int indexContent = cursor.getColumnIndex(TableDiary.COLUMN_CONTENT);

		final DiaryRecord record = serializer.read(cursor.getString(indexContent));

		final Versioned<DiaryRecord> item = new Versioned<>(record);
		item.setId(cursor.getString(indexID));
		item.setTimeStamp(Utils.parseTimeUTC(cursor.getString(indexTimestamp)));
		item.setHash(cursor.getString(indexHash));
		item.setVersion(cursor.getInt(indexVersion));
		item.setDeleted((cursor.getInt(indexDeleted) == 1));

		return item;
	}

	private List<Versioned<DiaryRecord>> extractRecords(Cursor cursor)
	{
		return extractRecords(cursor, 0);
	}

	public static class Verifier
	{
		public static boolean verifyRecords(List<Versioned<DiaryRecord>> records, Date start, Date end)
		{
			// null check #1
			if (records == null)
			{
				Log.e(TAG, "Records validation failed: records are null");
				return false;
			}

			// null check #2 / range check
			for (Versioned<DiaryRecord> record : records)
			{
				if (record == null)
				{
					Log.e(TAG, "Records validation failed: record list contain null items");
					return false;
				}

				if (record.getData().getTime().before(start) || record.getData().getTime().after(end))
				{
					Log.e(TAG,
							String.format("Records validation failed: time of item %s (%s) is out of time range (%s -- %s)", record.getId(),
									Utils.formatTimeUTC(record.getData().getTime()), Utils.formatTimeUTC(start), Utils.formatTimeUTC(end)));
					print(records);
					return false;
				}
			}

			// sorting check
			for (int i = 0; i < records.size() - 1; i++)
			{
				Date time1 = records.get(i).getData().getTime();
				Date time2 = records.get(i + 1).getData().getTime();
				if (time1.after(time2))
				{
					Log.e(TAG, String.format("Records validation failed: items %d and %d are wrong sorted", i, i + 1));
					print(records);
					return false;
				}
			}

			return true;
		}

		public static void print(List<Versioned<DiaryRecord>> records)
		{
			for (Versioned<DiaryRecord> item : records)
			{
				Log.e(TAG, String.format("ID %s %s", item.getId(), Utils.formatTimeUTC(item.getData().getTime())));
			}
		}
	}

	@Override
	public void importData(InputStream stream) throws IOException
	{
		new PlainDataImporter(context, TableDiary.INSTANCE, "6")
		{
			@Override
			protected void parseEntry(String[] items, ContentValues newValues)
			{
				if (items.length != 7)
				{
					throw new IllegalArgumentException("Invalid entry: " + Arrays.toString(items));
				}

				newValues.put(TableDiary.COLUMN_TIMECACHE, items[0]);
				newValues.put(TableDiary.COLUMN_ID, items[1]);
				newValues.put(TableDiary.COLUMN_TIMESTAMP, items[2]);
				newValues.put(TableDiary.COLUMN_HASH, items[3]);
				newValues.put(TableDiary.COLUMN_VERSION, Integer.parseInt(items[4]));
				newValues.put(TableDiary.COLUMN_DELETED, Boolean.parseBoolean(items[5]));
				newValues.put(TableDiary.COLUMN_CONTENT, items[6]);
			}
		}.importPlain(stream);
	}

	private void importFromJson(InputStream stream) throws IOException
	{
		StreamReader<Versioned<DiaryRecord>> reader = new DiaryRecordVersionedReader();

		try (JsonReader json = new JsonReader(new InputStreamReader(stream, StandardCharsets.UTF_8)))
		{
			SQLiteDatabase db = new MyDBHelper(context).getWritableDatabase();
			db.beginTransaction();
			try
			{
				json.beginArray();

				ContentValues newValues = new ContentValues();
				int count = 0;

				while (json.hasNext())
				{
					Versioned<DiaryRecord> record = reader.read(json);

					newValues.put(TableDiary.COLUMN_ID, record.getId());
					newValues.put(TableDiary.COLUMN_TIMESTAMP, Utils.formatTimeUTC(record.getTimeStamp()));
					newValues.put(TableDiary.COLUMN_HASH, record.getHash());
					newValues.put(TableDiary.COLUMN_VERSION, record.getVersion());
					newValues.put(TableDiary.COLUMN_DELETED, record.isDeleted());
					newValues.put(TableDiary.COLUMN_CONTENT, serializer.write(record.getData()));
					newValues.put(TableDiary.COLUMN_TIMECACHE, Utils.formatTimeUTC(record.getData().getTime()));

					db.insertWithOnConflict(TableDiary.TABLE_NAME, null, newValues, SQLiteDatabase.CONFLICT_IGNORE);

					if (++count % 1000 == 0)
					{
						db.setTransactionSuccessful();
						db.endTransaction();
						db.beginTransaction();
					}
				}
				json.endArray();

				db.setTransactionSuccessful();
			}
			finally
			{
				db.endTransaction();
				db.close();
				resolver.notifyChange(TableDiary.CONTENT_URI, null);
			}
		}
	}
}
