package org.bosik.diacomp.web.backend.features.diary.function;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Parser;
import org.bosik.diacomp.core.persistence.serializers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.utils.SerializerAdapter;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.mysql.MySQLAccess;

public class MySQLDiaryDAO implements DiaryDAO
{
	private final MySQLAccess				db			= new MySQLAccess();
	private final Parser<DiaryRecord>		parser		= new ParserDiaryRecord();
	private final Serializer<DiaryRecord>	serializer	= new SerializerAdapter<DiaryRecord>(parser);

	private static List<Versioned<String>> parseDiaryRecords(ResultSet resultSet) throws SQLException
	{
		// Don't parse to DiaryPage: we have to convert it into String in REST anyway

		List<Versioned<String>> result = new LinkedList<Versioned<String>>();

		while (resultSet.next())
		{
			String id = resultSet.getString(MySQLAccess.COLUMN_DIARY_GUID);
			Date timeStamp = Utils.parseTimeUTC(resultSet.getString(MySQLAccess.COLUMN_DIARY_TIMESTAMP));
			int version = resultSet.getInt(MySQLAccess.COLUMN_DIARY_VERSION);
			boolean deleted = (resultSet.getInt(MySQLAccess.COLUMN_DIARY_DELETED) == 1);
			String content = resultSet.getString(MySQLAccess.COLUMN_DIARY_CONTENT);

			Versioned<String> item = new Versioned<String>();
			item.setId(id);
			item.setTimeStamp(timeStamp);
			item.setVersion(version);
			item.setDeleted(deleted);
			// item.setData(serializer.read(content));
			item.setData(content);

			result.add(item);
		}

		return result;
	}

	@Override
	public Versioned<String> findByGuid(int userId, String guid)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s = '%s')", MySQLAccess.COLUMN_DIARY_USER, userId,
					MySQLAccess.COLUMN_DIARY_GUID, guid);

			ResultSet set = db.select(MySQLAccess.TABLE_DIARY, clause, null);
			List<Versioned<String>> result = parseDiaryRecords(set);
			set.close();
			return result.isEmpty() ? null : result.get(0);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<String>> findMod(int userId, Date time, boolean includeRemoved)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s >= '%s')", MySQLAccess.COLUMN_DIARY_USER, userId,
					MySQLAccess.COLUMN_DIARY_TIMESTAMP, Utils.formatTimeUTC(time));

			if (!includeRemoved)
			{
				clause += String.format(" AND (%s = '%s')", MySQLAccess.COLUMN_DIARY_DELETED,
						Utils.formatBooleanInt(false));
			}

			String order = MySQLAccess.COLUMN_DIARY_TIMECACHE;

			ResultSet set = db.select(MySQLAccess.TABLE_DIARY, clause, order);
			List<Versioned<String>> result = parseDiaryRecords(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<String>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s >= '%s') AND (%s <= '%s')", MySQLAccess.COLUMN_DIARY_USER,
					userId, MySQLAccess.COLUMN_DIARY_TIMECACHE, Utils.formatTimeUTC(startTime),
					MySQLAccess.COLUMN_DIARY_TIMECACHE, Utils.formatTimeUTC(endTime));

			if (!includeRemoved)
			{
				clause += String.format(" AND (%s = '%s')", MySQLAccess.COLUMN_DIARY_DELETED,
						Utils.formatBooleanInt(false));
			}

			String order = MySQLAccess.COLUMN_DIARY_TIMECACHE;

			// System.out.println("Requesting SQL clause: " + clause);
			ResultSet set = db.select(MySQLAccess.TABLE_DIARY, clause, order);
			List<Versioned<String>> result = parseDiaryRecords(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public void post(int userId, List<Versioned<DiaryRecord>> records)
	{
		try
		{
			for (Versioned<DiaryRecord> item : records)
			{
				final String content = serializer.write(item.getData());
				final String timeCache = Utils.formatTimeUTC(item.getData().getTime());
				final String timeStamp = Utils.formatTimeUTC(item.getTimeStamp());
				final String version = String.valueOf(item.getVersion());
				final String deleted = Utils.formatBooleanInt(item.isDeleted());

				if (findByGuid(userId, item.getId()) != null)
				{
					// presented, update

					SortedMap<String, String> set = new TreeMap<String, String>();
					set.put(MySQLAccess.COLUMN_DIARY_TIMESTAMP, timeStamp);
					set.put(MySQLAccess.COLUMN_DIARY_VERSION, version);
					set.put(MySQLAccess.COLUMN_DIARY_DELETED, deleted);
					set.put(MySQLAccess.COLUMN_DIARY_CONTENT, content);
					set.put(MySQLAccess.COLUMN_DIARY_TIMECACHE, timeCache);

					SortedMap<String, String> where = new TreeMap<String, String>();
					where.put(MySQLAccess.COLUMN_DIARY_GUID, item.getId());
					where.put(MySQLAccess.COLUMN_DIARY_USER, String.valueOf(userId));

					db.update(MySQLAccess.TABLE_DIARY, set, where);
				}
				else
				{
					// not presented, insert

					LinkedHashMap<String, String> set = new LinkedHashMap<String, String>();
					set.put(MySQLAccess.COLUMN_DIARY_GUID, item.getId());
					set.put(MySQLAccess.COLUMN_DIARY_USER, String.valueOf(userId));
					set.put(MySQLAccess.COLUMN_DIARY_TIMESTAMP, timeStamp);
					set.put(MySQLAccess.COLUMN_DIARY_VERSION, version);
					set.put(MySQLAccess.COLUMN_DIARY_DELETED, deleted);
					set.put(MySQLAccess.COLUMN_DIARY_CONTENT, content);
					set.put(MySQLAccess.COLUMN_DIARY_TIMECACHE, timeCache);

					db.insert(MySQLAccess.TABLE_DIARY, set);
				}
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}
}
