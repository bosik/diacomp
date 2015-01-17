package org.bosik.diacomp.android.backend.features.diary;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider_v1;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
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
			throw new NullPointerException("Content Resolver can't be null");
		}
		this.resolver = resolver;
	}

	/* ============================ API ============================ */

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
		save(Arrays.<Versioned<DiaryRecord>> asList(item));
	}

	@Override
	public Versioned<DiaryRecord> findById(String guid) throws CommonServiceException
	{
		// construct parameters
		String[] projection = { DiaryContentProvider_v1.COLUMN_DIARY_GUID, DiaryContentProvider_v1.COLUMN_DIARY_TIMESTAMP,
				DiaryContentProvider_v1.COLUMN_DIARY_VERSION, DiaryContentProvider_v1.COLUMN_DIARY_DELETED,
				DiaryContentProvider_v1.COLUMN_DIARY_CONTENT, DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE };

		String clause = DiaryContentProvider_v1.COLUMN_DIARY_GUID + " = ?";
		String[] clauseArgs = { guid };

		String sortOrder = null;// DiaryContentProvider.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider_v1.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		List<Versioned<DiaryRecord>> recs = extractRecords(cursor);

		return recs.isEmpty() ? null : recs.get(0);
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date since) throws CommonServiceException
	{
		// construct parameters
		String[] projection = { DiaryContentProvider_v1.COLUMN_DIARY_GUID, DiaryContentProvider_v1.COLUMN_DIARY_TIMESTAMP,
				DiaryContentProvider_v1.COLUMN_DIARY_VERSION, DiaryContentProvider_v1.COLUMN_DIARY_DELETED,
				DiaryContentProvider_v1.COLUMN_DIARY_CONTENT, DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE };

		String clause = String.format("%s > ?", DiaryContentProvider_v1.COLUMN_DIARY_TIMESTAMP);
		String[] clauseArgs = new String[] { Utils.formatTimeUTC(since) };

		String sortOrder = DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider_v1.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		return extractRecords(cursor);
	}

	@Override
	public synchronized List<Versioned<DiaryRecord>> findPeriod(Date startTime, Date endTime, boolean includeRemoved)
			throws CommonServiceException
	{
		if (startTime == null)
		{
			throw new NullPointerException("startTime is null");
		}

		if (endTime == null)
		{
			throw new NullPointerException("endTime is null");
		}

		// construct parameters
		String[] projection = { DiaryContentProvider_v1.COLUMN_DIARY_GUID, DiaryContentProvider_v1.COLUMN_DIARY_TIMESTAMP,
				DiaryContentProvider_v1.COLUMN_DIARY_VERSION, DiaryContentProvider_v1.COLUMN_DIARY_DELETED,
				DiaryContentProvider_v1.COLUMN_DIARY_CONTENT, DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE };

		String clause;
		String[] clauseArgs;

		if (includeRemoved)
		{
			clause = String.format("(%s >= ?) AND (%s <= ?)", DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE,
					DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE);
			clauseArgs = new String[] { Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime) };
		}
		else
		{
			clause = String.format("(%s >= ?) AND (%s <= ?) AND (%s = 0)", DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE,
					DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE, DiaryContentProvider_v1.COLUMN_DIARY_DELETED);
			clauseArgs = new String[] { Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime) };
		}

		String sortOrder = DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider_v1.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		List<Versioned<DiaryRecord>> records = extractRecords(cursor);

		Log.d(TAG, String.format("#DBF %d items found between %s and %s", records.size(),
				Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime)));

		// detailed logging inside
		Verifier.verifyRecords(records, startTime, endTime);

		return records;
	}

	private boolean recordExists(String guid)
	{
		// construct parameters
		String[] projection = { DiaryContentProvider_v1.COLUMN_DIARY_GUID };
		String clause = DiaryContentProvider_v1.COLUMN_DIARY_GUID + " = ?";
		String[] clauseArgs = { guid };
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider_v1.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		return cursor.moveToFirst();
	}

	@Override
	public void save(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		try
		{
			for (Versioned<DiaryRecord> record : records)
			{
				String content = serializer.write(record.getData());
				boolean exists = recordExists(record.getId());

				ContentValues newValues = new ContentValues();

				newValues.put(DiaryContentProvider_v1.COLUMN_DIARY_TIMESTAMP, Utils.formatTimeUTC(record.getTimeStamp()));
				newValues.put(DiaryContentProvider_v1.COLUMN_DIARY_VERSION, record.getVersion());
				newValues.put(DiaryContentProvider_v1.COLUMN_DIARY_DELETED, record.isDeleted());
				newValues.put(DiaryContentProvider_v1.COLUMN_DIARY_CONTENT, content);
				newValues.put(DiaryContentProvider_v1.COLUMN_DIARY_TIMECACHE,
						Utils.formatTimeUTC(record.getData().getTime()));

				if (exists)
				{
					Log.v(TAG, "Updating item " + record.getId() + ": " + content);

					String clause = DiaryContentProvider_v1.COLUMN_DIARY_GUID + " = ?";
					String[] args = new String[] { record.getId() };
					resolver.update(DiaryContentProvider_v1.CONTENT_DIARY_URI, newValues, clause, args);
				}
				else
				{
					Log.v(TAG, "Inserting item " + record.getId() + ": " + content);

					newValues.put(DiaryContentProvider_v1.COLUMN_DIARY_GUID, record.getId());
					resolver.insert(DiaryContentProvider_v1.CONTENT_DIARY_URI, newValues);
				}
			}
		}
		catch (Exception e)
		{
			throw new PersistenceException(e);
		}
	}

	public void deletePermanently(String id) throws CommonServiceException
	{
		try
		{
			String clause = DiaryContentProvider_v1.COLUMN_DIARY_GUID + " = ?";
			String[] args = new String[] { id };
			resolver.delete(DiaryContentProvider_v1.CONTENT_DIARY_URI, clause, args);
		}
		catch (Exception e)
		{
			throw new PersistenceException(e);
		}
	}

	/* ======================= ROUTINES ========================= */

	private List<Versioned<DiaryRecord>> extractRecords(Cursor cursor)
	{
		if (cursor != null)
		{
			int indexGUID = cursor.getColumnIndex(DiaryContentProvider_v1.COLUMN_DIARY_GUID);
			int indexTimestamp = cursor.getColumnIndex(DiaryContentProvider_v1.COLUMN_DIARY_TIMESTAMP);
			int indexVersion = cursor.getColumnIndex(DiaryContentProvider_v1.COLUMN_DIARY_VERSION);
			int indexDeleted = cursor.getColumnIndex(DiaryContentProvider_v1.COLUMN_DIARY_DELETED);
			int indexContent = cursor.getColumnIndex(DiaryContentProvider_v1.COLUMN_DIARY_CONTENT);

			List<Versioned<DiaryRecord>> res = new ArrayList<Versioned<DiaryRecord>>();

			while (cursor.moveToNext())
			{
				String guid = cursor.getString(indexGUID);
				Date timestamp = Utils.parseTimeUTC(cursor.getString(indexTimestamp));
				int version = cursor.getInt(indexVersion);
				boolean deleted = (cursor.getInt(indexDeleted) == 1);
				String content = cursor.getString(indexContent);
				DiaryRecord record = serializer.read(content);

				Versioned<DiaryRecord> item = new Versioned<DiaryRecord>(record);
				item.setId(guid);
				item.setTimeStamp(timestamp);
				item.setVersion(version);
				item.setDeleted(deleted);

				res.add(item);

				Log.v(TAG, String.format("#DBF Extracted item #%s: %s", guid, content));
			}

			return res;
		}
		else
		{
			throw new CommonServiceException(new NullPointerException("Cursor is null"));
		}
	}

	// 2014-08-17 Commented out as unused

	// private static String formatList(List<?> list)
	// {
	// StringBuilder sb = new StringBuilder();
	//
	// sb.append("(");
	// for (int i = 0; i < list.size(); i++)
	// {
	// sb.append("\"");
	// sb.append(list.get(i));
	// sb.append("\"");
	// if (i < (list.size() - 1))
	// {
	// sb.append(", ");
	// }
	// }
	// sb.append(")");
	//
	// return sb.toString();
	// }

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