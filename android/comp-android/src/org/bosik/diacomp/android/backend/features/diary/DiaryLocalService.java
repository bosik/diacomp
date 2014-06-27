package org.bosik.diacomp.android.backend.features.diary;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
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
	private static final String				TAG			= DiaryLocalService.class.getSimpleName();

	/* ============================ FIELDS ============================ */

	// private Parser<DiaryRecord> parser = new ParserDiaryRecord();
	// private Parser<Versioned<DiaryRecord>> parserV = new ParserVersioned<DiaryRecord>(parser);
	// private Serializer<Versioned<DiaryRecord>> serializer = new
	// SerializerAdapter<Versioned<DiaryRecord>>(parserV);

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
	public Versioned<DiaryRecord> findById(String guid) throws CommonServiceException
	{
		// construct parameters
		String[] projection = { DiaryContentProvider.COLUMN_DIARY_GUID, DiaryContentProvider.COLUMN_DIARY_TIMESTAMP,
				DiaryContentProvider.COLUMN_DIARY_VERSION, DiaryContentProvider.COLUMN_DIARY_DELETED,
				DiaryContentProvider.COLUMN_DIARY_CONTENT, DiaryContentProvider.COLUMN_DIARY_TIMECACHE };

		String clause = DiaryContentProvider.COLUMN_DIARY_GUID + " = ?";
		String[] clauseArgs = { guid };

		String sortOrder = null;// DiaryContentProvider.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		List<Versioned<DiaryRecord>> recs = extractRecords(cursor);

		return recs.isEmpty() ? null : recs.get(0);
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date since) throws CommonServiceException
	{
		// construct parameters
		String[] projection = { DiaryContentProvider.COLUMN_DIARY_GUID, DiaryContentProvider.COLUMN_DIARY_TIMESTAMP,
				DiaryContentProvider.COLUMN_DIARY_VERSION, DiaryContentProvider.COLUMN_DIARY_DELETED,
				DiaryContentProvider.COLUMN_DIARY_CONTENT, DiaryContentProvider.COLUMN_DIARY_TIMECACHE };

		String clause = String.format("%s > ?", DiaryContentProvider.COLUMN_DIARY_TIMESTAMP);
		String[] clauseArgs = new String[] { Utils.formatTimeUTC(since) };

		String sortOrder = DiaryContentProvider.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		return extractRecords(cursor);
	}

	@Override
	public synchronized List<Versioned<DiaryRecord>> findBetween(Date fromDate, Date toDate, boolean includeRemoved)
			throws CommonServiceException
	{
		if (fromDate == null)
		{
			throw new NullPointerException("fromDate is null");
		}

		if (toDate == null)
		{
			throw new NullPointerException("toDate is null");
		}

		Log.d(TAG,
				String.format("#DBF Searching for items between %s and %s", Utils.formatTimeUTC(fromDate),
						Utils.formatTimeUTC(toDate)));

		// construct parameters
		String[] projection = { DiaryContentProvider.COLUMN_DIARY_GUID, DiaryContentProvider.COLUMN_DIARY_TIMESTAMP,
				DiaryContentProvider.COLUMN_DIARY_VERSION, DiaryContentProvider.COLUMN_DIARY_DELETED,
				DiaryContentProvider.COLUMN_DIARY_CONTENT, DiaryContentProvider.COLUMN_DIARY_TIMECACHE };

		String clause;
		String[] clauseArgs;

		if (includeRemoved)
		{
			clause = String.format("(%s >= ?) AND (%s <= ?)", DiaryContentProvider.COLUMN_DIARY_TIMECACHE,
					DiaryContentProvider.COLUMN_DIARY_TIMECACHE);
			clauseArgs = new String[] { Utils.formatTimeUTC(fromDate), Utils.formatTimeUTC(toDate) };
		}
		else
		{
			clause = String.format("(%s >= ?) AND (%s <= ?) AND (%s = 0)", DiaryContentProvider.COLUMN_DIARY_TIMECACHE,
					DiaryContentProvider.COLUMN_DIARY_TIMECACHE, DiaryContentProvider.COLUMN_DIARY_DELETED);
			clauseArgs = new String[] { Utils.formatTimeUTC(fromDate), Utils.formatTimeUTC(toDate) };
		}
		// String clause;
		// String[] clauseArgs;
		//
		// if (includeModified)
		// {
		// clause = String.format("%s > ?", DiaryContentProvider.COLUMN_DIARY_TIMESTAMP);
		// clauseArgs = new String[] { Utils.formatTimeUTC(time) };
		// }
		// else
		// {
		// clause = String.format("(%s > ?) AND (%s = 0)",
		// DiaryContentProvider.COLUMN_DIARY_TIMESTAMP,
		// DiaryContentProvider.COLUMN_DIARY_DELETED);
		// clauseArgs = new String[] { Utils.formatTimeUTC(time) };
		// }

		String sortOrder = DiaryContentProvider.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		return extractRecords(cursor);
	}

	private boolean recordExists(String guid)
	{
		// construct parameters
		String[] projection = { DiaryContentProvider.COLUMN_DIARY_GUID };
		String clause = DiaryContentProvider.COLUMN_DIARY_GUID + " = ?";
		String[] clauseArgs = { guid };
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
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

				newValues.put(DiaryContentProvider.COLUMN_DIARY_TIMESTAMP, Utils.formatTimeUTC(record.getTimeStamp()));
				newValues.put(DiaryContentProvider.COLUMN_DIARY_VERSION, record.getVersion());
				newValues.put(DiaryContentProvider.COLUMN_DIARY_DELETED, record.isDeleted());
				newValues.put(DiaryContentProvider.COLUMN_DIARY_CONTENT, content);
				newValues.put(DiaryContentProvider.COLUMN_DIARY_TIMECACHE,
						Utils.formatTimeUTC(record.getData().getTime()));

				if (exists)
				{
					Log.v(TAG, "Updating item " + record.getId() + ": " + content);

					String clause = DiaryContentProvider.COLUMN_DIARY_GUID + " = ?";
					String[] args = new String[] { record.getId() };
					resolver.update(DiaryContentProvider.CONTENT_DIARY_URI, newValues, clause, args);
				}
				else
				{
					Log.v(TAG, "Inserting item " + record.getId() + ": " + content);

					newValues.put(DiaryContentProvider.COLUMN_DIARY_GUID, record.getId());
					resolver.insert(DiaryContentProvider.CONTENT_DIARY_URI, newValues);
				}
			}
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
			int indexGUID = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_GUID);
			int indexTimestamp = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_TIMESTAMP);
			int indexVersion = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_VERSION);
			int indexDeleted = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_DELETED);
			int indexContent = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_CONTENT);

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

			Log.d(TAG, String.format("#DBF Extracted %d items", res.size()));
			return res;
		}
		else
		{
			throw new CommonServiceException(new NullPointerException("Cursor is null"));
		}
	}

	private static String formatList(List<?> list)
	{
		StringBuilder sb = new StringBuilder();

		sb.append("(");
		for (int i = 0; i < list.size(); i++)
		{
			sb.append("\"");
			sb.append(list.get(i));
			sb.append("\"");
			if (i < (list.size() - 1))
			{
				sb.append(", ");
			}
		}
		sb.append(")");

		return sb.toString();
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
		save(Arrays.<Versioned<DiaryRecord>> asList(item));
	}
}