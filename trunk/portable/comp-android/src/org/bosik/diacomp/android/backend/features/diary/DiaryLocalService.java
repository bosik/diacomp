package org.bosik.diacomp.android.backend.features.diary;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.services.sync.HashUtils;
import org.bosik.diacomp.core.services.sync.MemoryMerkleTree;
import org.bosik.diacomp.core.services.sync.MerkleTree;
import org.bosik.diacomp.core.utils.Profiler;
import org.bosik.diacomp.core.utils.Utils;
import android.app.Activity;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;
import android.util.Log;

@SuppressWarnings("unchecked")
public class DiaryLocalService implements DiaryService
{
	static final String						TAG			= DiaryLocalService.class.getSimpleName();

	/* ============================ FIELDS ============================ */

	private final ContentResolver			resolver;
	private final Parser<DiaryRecord>		parser		= new ParserDiaryRecord();
	private final Serializer<DiaryRecord>	serializer	= new SerializerAdapter<DiaryRecord>(parser);

	/* ============================ CONSTRUCTOR ============================ */

	/**
	 * Constructor
	 * 
	 * @param resolver
	 *            Content resolver; might be accessed by {@link Activity#getContentResolver()}
	 */
	public DiaryLocalService(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new IllegalArgumentException("Content Resolver is null");
		}
		this.resolver = resolver;
	}

	/* ============================ API ============================ */

	@Override
	public int count(String prefix)
	{
		if (prefix == null)
		{
			throw new IllegalArgumentException("ID prefix is null");
		}

		String[] projection = new String[] { "count(*) AS count" };
		String clause = String.format("%s LIKE ?", DiaryContentProvider.COLUMN_DIARY_GUID);
		String[] clauseArgs = { prefix + "%" };

		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs, null);

		try
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
		finally
		{
			if (cursor != null)
			{
				cursor.close();
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
		item.updateTimeStamp();
		save(Arrays.<Versioned<DiaryRecord>> asList(item));
	}

	@Override
	public Versioned<DiaryRecord> findById(String id) throws CommonServiceException
	{
		// construct parameters
		String[] projection = null; // all
		String clause = DiaryContentProvider.COLUMN_DIARY_GUID + " = ?";
		String[] clauseArgs = { id };
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		List<Versioned<DiaryRecord>> recs = extractRecords(cursor);

		return recs.isEmpty() ? null : recs.get(0);
	}

	@Override
	public List<Versioned<DiaryRecord>> findByIdPrefix(String prefix)
	{
		// construct parameters
		String[] projection = null; // all
		String clause = String.format("%s LIKE ?", DiaryContentProvider.COLUMN_DIARY_GUID);
		String[] clauseArgs = { prefix + "%" };
		String sortOrder = DiaryContentProvider.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		return extractRecords(cursor, MAX_ITEMS_COUNT);
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date since) throws CommonServiceException
	{
		// construct parameters
		String[] projection = null;
		String clause = String.format("%s > ?", DiaryContentProvider.COLUMN_DIARY_TIMESTAMP);
		String[] clauseArgs = { Utils.formatTimeUTC(since) };
		String sortOrder = DiaryContentProvider.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);
		return extractRecords(cursor, MAX_ITEMS_COUNT);
	}

	@Override
	public synchronized List<Versioned<DiaryRecord>> findPeriod(Date startTime, Date endTime, boolean includeRemoved)
			throws CommonServiceException
	{
		long time = System.currentTimeMillis();

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
			clause = String.format("(%s >= ?) AND (%s < ?)", DiaryContentProvider.COLUMN_DIARY_TIMECACHE,
					DiaryContentProvider.COLUMN_DIARY_TIMECACHE);
			clauseArgs = new String[] { Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime) };
		}
		else
		{
			clause = String.format("(%s >= ?) AND (%s < ?) AND (%s = 0)", DiaryContentProvider.COLUMN_DIARY_TIMECACHE,
					DiaryContentProvider.COLUMN_DIARY_TIMECACHE, DiaryContentProvider.COLUMN_DIARY_DELETED);
			clauseArgs = new String[] { Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime) };
		}

		String sortOrder = DiaryContentProvider.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		List<Versioned<DiaryRecord>> records = extractRecords(cursor);

		// detailed logging inside
		Verifier.verifyRecords(records, startTime, endTime);

		time = System.currentTimeMillis() - time;

		Log.d(TAG,
				String.format("#DBF %d items found between %s and %s in %d ms", records.size(),
						Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime), time));

		return records;
	}

	private boolean recordExists(String id)
	{
		final String[] select = { DiaryContentProvider.COLUMN_DIARY_GUID };
		final String where = String.format("%s = ?", DiaryContentProvider.COLUMN_DIARY_GUID);
		final String[] whereArgs = { id };
		final String sortOrder = null;

		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, select, where, whereArgs, sortOrder);

		try
		{
			return cursor != null && cursor.moveToFirst();
		}
		finally
		{
			cursor.close();
		}
	}

	@Override
	public String getHash(String prefix) throws CommonServiceException
	{
		try
		{
			// constructing parameters
			final String[] select = { DiaryContentProvider.COLUMN_DIARY_HASH_HASH };
			final String where = DiaryContentProvider.COLUMN_DIARY_HASH_GUID + " = ?";
			final String[] whereArgs = { prefix };

			// execute query
			Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_HASH_URI, select, where, whereArgs, null);

			// analyze response
			if (cursor != null)
			{
				int indexHash = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_HASH_HASH);
				String hash = null;

				if (cursor.moveToNext())
				{
					hash = cursor.getString(indexHash);
				}

				cursor.close();
				return hash;
			}
			else
			{
				throw new IllegalArgumentException("Cursor is null");
			}
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public Map<String, String> getHashChildren(String prefix) throws CommonServiceException
	{
		try
		{
			if (prefix.length() < ObjectService.ID_PREFIX_SIZE)
			{
				// constructing parameters
				final String[] select = { DiaryContentProvider.COLUMN_DIARY_HASH_GUID,
						DiaryContentProvider.COLUMN_DIARY_HASH_HASH };
				final String where = DiaryContentProvider.COLUMN_DIARY_HASH_GUID + " LIKE ?";
				final String[] whereArgs = { prefix + "_" };

				// execute query
				Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_HASH_URI, select, where, whereArgs,
						null);

				// analyze response
				if (cursor != null)
				{
					int indexId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_HASH_GUID);
					int indexHash = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_HASH_HASH);

					Map<String, String> result = new HashMap<String, String>();

					while (cursor.moveToNext())
					{
						String id = cursor.getString(indexId);
						String hash = cursor.getString(indexHash);
						result.put(id, hash);
					}

					cursor.close();

					return result;
				}
				else
				{
					throw new IllegalArgumentException("Cursor is null");
				}
			}
			else
			{
				// constructing parameters
				final String[] select = { DiaryContentProvider.COLUMN_DIARY_GUID,
						DiaryContentProvider.COLUMN_DIARY_HASH };
				final String where = String.format("%s LIKE ?", DiaryContentProvider.COLUMN_DIARY_GUID);
				final String[] whereArgs = { prefix + "%" };

				// execute query
				Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, select, where, whereArgs, null);

				// analyze response
				if (cursor != null)
				{
					int indexId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_GUID);
					int indexHash = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_HASH);

					Map<String, String> result = new HashMap<String, String>();

					while (cursor.moveToNext())
					{
						String id = cursor.getString(indexId);
						String hash = cursor.getString(indexHash);
						result.put(id, hash);
					}

					cursor.close();

					return result;
				}
				else
				{
					throw new IllegalArgumentException("Cursor is null");
				}
			}
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void save(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		try
		{
			for (Versioned<DiaryRecord> record : records)
			{
				if (verify(record))
				{
					String content = serializer.write(record.getData());
					boolean exists = recordExists(record.getId());

					ContentValues newValues = new ContentValues();

					newValues.put(DiaryContentProvider.COLUMN_DIARY_TIMESTAMP,
							Utils.formatTimeUTC(record.getTimeStamp()));
					newValues.put(DiaryContentProvider.COLUMN_DIARY_HASH, record.getHash());
					newValues.put(DiaryContentProvider.COLUMN_DIARY_VERSION, record.getVersion());
					newValues.put(DiaryContentProvider.COLUMN_DIARY_DELETED, record.isDeleted());
					newValues.put(DiaryContentProvider.COLUMN_DIARY_CONTENT, content);
					newValues.put(DiaryContentProvider.COLUMN_DIARY_TIMECACHE,
							Utils.formatTimeUTC(record.getData().getTime()));

					if (exists)
					{
						Log.v(TAG, "Updating item " + record.getId() + ": " + content);

						String clause = DiaryContentProvider.COLUMN_DIARY_GUID + " = ?";
						String[] args = { record.getId() };
						resolver.update(DiaryContentProvider.CONTENT_DIARY_URI, newValues, clause, args);
					}
					else
					{
						Log.v(TAG, "Inserting item " + record.getId() + ": " + content);

						newValues.put(DiaryContentProvider.COLUMN_DIARY_GUID, record.getId());
						resolver.insert(DiaryContentProvider.CONTENT_DIARY_URI, newValues);
					}
				}
				else
				{
					Log.e(TAG, "Invalid record: " + record);
				}
			}
		}
		catch (Exception e)
		{
			throw new PersistenceException(e);
		}
	}

	private static boolean verify(Versioned<DiaryRecord> record)
	{
		return (record != null && record.getId() != null && record.getId().length() == ObjectService.ID_FULL_SIZE);
	}

	public void deletePermanently(String id) throws CommonServiceException
	{
		try
		{
			String clause = DiaryContentProvider.COLUMN_DIARY_GUID + " = ?";
			String[] args = { id };
			resolver.delete(DiaryContentProvider.CONTENT_DIARY_URI, clause, args);
		}
		catch (Exception e)
		{
			throw new PersistenceException(e);
		}
	}

	/**
	 * Returns sorted map (ID, Hash) for all items
	 * 
	 * @return
	 */
	private SortedMap<String, String> getDataHashes()
	{
		// constructing parameters
		final String[] select = { DiaryContentProvider.COLUMN_DIARY_GUID, DiaryContentProvider.COLUMN_DIARY_HASH };
		final String where = null;
		final String[] whereArgs = null;

		// execute query
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, select, where, whereArgs, null);

		// analyze response
		int indexId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_GUID);
		int indexHash = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_HASH);

		SortedMap<String, String> result = new TreeMap<String, String>();

		while (cursor.moveToNext())
		{
			String id = cursor.getString(indexId);
			String hash = cursor.getString(indexHash);

			if (id == null || id.length() < ObjectService.ID_PREFIX_SIZE)
			{
				Log.w(TAG, String.format("Invalid hash ignored: %s = %s", id, hash));
			}
			else
			{
				// THINK: probably storing entries is unnecessary, so we should process it as we go
				result.put(id, hash);
			}
		}

		cursor.close();
		return result;
	}

	public synchronized void rebuildHashTree()
	{
		// processing data
		SortedMap<String, String> hashes = getDataHashes();
		SortedMap<String, String> tree = HashUtils.buildHashTree(hashes);

		// clearing
		resolver.delete(DiaryContentProvider.CONTENT_DIARY_HASH_URI, null, null);

		// persisting new values
		for (Entry<String, String> entry : tree.entrySet())
		{
			ContentValues newValues = new ContentValues();
			newValues.put(DiaryContentProvider.COLUMN_DIARY_HASH_GUID, entry.getKey());
			newValues.put(DiaryContentProvider.COLUMN_DIARY_HASH_HASH, entry.getValue());
			resolver.insert(DiaryContentProvider.CONTENT_DIARY_HASH_URI, newValues);
		}
	}

	@Override
	public MerkleTree getHashTree()
	{
		/**/Profiler p = new Profiler();

		SortedMap<String, String> hashes = getDataHashes();
		/**/Log.d(TAG, "getDataHashes(): " + p.sinceLastCheck() / 1000000 + " ms");

		SortedMap<String, String> tree = HashUtils.buildHashTree(hashes);
		/**/Log.d(TAG, "buildHashTree(): " + p.sinceLastCheck() / 1000000 + " ms");

		MemoryMerkleTree result = new MemoryMerkleTree();
		result.putAll(tree); // headers (0..4 chars id)
		result.putAll(hashes); // leafs (32 chars id)

		/**/Log.d(TAG, "getHashTree() [total]: " + p.sinceStart() / 1000000 + " ms");

		return result;
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
			int indexID = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_GUID);
			int indexTimestamp = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_TIMESTAMP);
			int indexHash = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_HASH);
			int indexVersion = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_VERSION);
			int indexDeleted = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_DELETED);
			int indexContent = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_CONTENT);

			List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();

			while (cursor.moveToNext())
			{
				String id = cursor.getString(indexID);
				Date timestamp = Utils.parseTimeUTC(cursor.getString(indexTimestamp));
				String hash = cursor.getString(indexHash);
				int version = cursor.getInt(indexVersion);
				boolean deleted = (cursor.getInt(indexDeleted) == 1);
				String content = cursor.getString(indexContent);
				DiaryRecord record = serializer.read(content);

				Versioned<DiaryRecord> item = new Versioned<DiaryRecord>(record);
				item.setId(id);
				item.setTimeStamp(timestamp);
				item.setHash(hash);
				item.setVersion(version);
				item.setDeleted(deleted);

				result.add(item);

				Log.v(TAG, String.format("#DBF Extracted item #%s: %s", id, content));

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
					Log.e(TAG, String.format(
							"Records validation failed: time of item %s (%s) is out of time range (%s -- %s)",
							record.getId(), Utils.formatTimeUTC(record.getData().getTime()),
							Utils.formatTimeUTC(start), Utils.formatTimeUTC(end)));
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

			Log.i(TAG, "Diary records successfully verified");
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
}