package org.bosik.diacomp.persistence.services.local;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.serializers.Parser;
import org.bosik.diacomp.persistence.serializers.ParserDiaryRecord;
import org.bosik.diacomp.persistence.serializers.Serializer;
import org.bosik.diacomp.persistence.serializers.utils.SerializerAdapter;
import org.bosik.diacomp.persistence.services.local.utils.DiaryContentProvider;
import org.bosik.diacomp.services.DiaryService;
import org.bosik.diacomp.services.exceptions.CommonServiceException;
import org.bosik.diacomp.services.exceptions.PersistenceException;
import org.bosik.diacomp.utils.Utils;
import android.app.Activity;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;

public class LocalDiaryService implements DiaryService
{
	// private static final String TAG = LocalDiaryService.class.getSimpleName();

	/* ============================ FIELDS ============================ */

	// private Parser<DiaryRecord> parser = new ParserDiaryRecord();
	// private Parser<Versioned<DiaryRecord>> parserV = new ParserVersioned<DiaryRecord>(parser);
	// private Serializer<Versioned<DiaryRecord>> serializer = new
	// SerializerAdapter<Versioned<DiaryRecord>>(parserV);

	private ContentResolver			resolver;
	private Parser<DiaryRecord>		parser		= new ParserDiaryRecord();
	private Serializer<DiaryRecord>	serializer	= new SerializerAdapter<DiaryRecord>(parser);

	/* ============================ CONSTRUCTOR ============================ */

	/**
	 * Constructor
	 * 
	 * @param resolver
	 *            Content resolver; might be accessed by {@link Activity#getContentResolver()}
	 */
	public LocalDiaryService(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new NullPointerException("Content Resolver can't be null");
		}
		this.resolver = resolver;
	}

	/* ============================ API ============================ */

	@Override
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException
	{
		// construct parameters
		String[] projection = { DiaryContentProvider.COLUMN_DIARY_GUID, DiaryContentProvider.COLUMN_DIARY_TIMESTAMP,
				DiaryContentProvider.COLUMN_DIARY_VERSION, DiaryContentProvider.COLUMN_DIARY_DELETED,
				DiaryContentProvider.COLUMN_DIARY_CONTENT, DiaryContentProvider.COLUMN_DIARY_TIMECACHE };

		String clause = DiaryContentProvider.COLUMN_DIARY_GUID + " in " + formatList(guids);
		String[] clauseArgs = {};

		String sortOrder = null;// DiaryContentProvider.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		List<Versioned<DiaryRecord>> recs = extractRecords(cursor);

		// making the order the same as requested GUID's order
		List<Versioned<DiaryRecord>> result = new LinkedList<Versioned<DiaryRecord>>();
		for (String guid : guids)
		{
			for (Versioned<DiaryRecord> rec : recs)
			{
				if (guid.equals(rec.getId()))
				{
					result.add(rec);
					break;
				}
			}
		}

		return result;
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date time, boolean includeRemoved) throws CommonServiceException
	{
		// construct parameters
		String[] projection = { DiaryContentProvider.COLUMN_DIARY_GUID, DiaryContentProvider.COLUMN_DIARY_TIMESTAMP,
				DiaryContentProvider.COLUMN_DIARY_VERSION, DiaryContentProvider.COLUMN_DIARY_DELETED,
				DiaryContentProvider.COLUMN_DIARY_CONTENT, DiaryContentProvider.COLUMN_DIARY_TIMECACHE };

		String clause;
		String[] clauseArgs;

		if (includeRemoved)
		{
			clause = String.format("%s > ?", DiaryContentProvider.COLUMN_DIARY_TIMESTAMP);
			clauseArgs = new String[] { Utils.formatTimeUTC(time) };
		}
		else
		{
			clause = String.format("(%s > ?) AND (%s = 0)", DiaryContentProvider.COLUMN_DIARY_TIMESTAMP,
					DiaryContentProvider.COLUMN_DIARY_DELETED);
			clauseArgs = new String[] { Utils.formatTimeUTC(time) };
		}

		String sortOrder = DiaryContentProvider.COLUMN_DIARY_TIMECACHE + " ASC";

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, projection, clause, clauseArgs,
				sortOrder);

		return extractRecords(cursor);
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date fromDate, Date toDate, boolean includeRemoved)
			throws CommonServiceException
	{
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
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
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
					String clause = DiaryContentProvider.COLUMN_DIARY_GUID + " = ?";
					String[] args = new String[] { record.getId() };
					resolver.update(DiaryContentProvider.CONTENT_DIARY_URI, newValues, clause, args);
				}
				else
				{
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
			}

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
}