package org.bosik.diacomp.web.backend.features.diary.function;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.mysql.MySQLAccess;
import org.bosik.diacomp.web.backend.features.auth.service.AuthService;
import org.bosik.diacomp.web.backend.features.auth.service.FrontendAuthService;
import org.springframework.stereotype.Service;

@Service
public class DiaryLocalService implements DiaryService
{
	// Diary table
	private static final String				TABLE_DIARY				= "diary2";
	private static final String				COLUMN_DIARY_USER		= "_UserID";
	private static final String				COLUMN_DIARY_GUID		= "_GUID";
	private static final String				COLUMN_DIARY_TIMESTAMP	= "_TimeStamp";
	private static final String				COLUMN_DIARY_HASH		= "_Hash";
	private static final String				COLUMN_DIARY_VERSION	= "_Version";
	private static final String				COLUMN_DIARY_DELETED	= "_Deleted";
	private static final String				COLUMN_DIARY_CONTENT	= "_Content";
	private static final String				COLUMN_DIARY_TIMECACHE	= "_TimeCache";

	// Diary hash table
	private static final String				TABLE_DIARY_HASH		= "diary_hash";
	private static final String				COLUMN_DIARY_HASH_USER	= "_UserID";
	private static final String				COLUMN_DIARY_HASH_GUID	= "_GUID";
	private static final String				COLUMN_DIARY_HASH_HASH	= "_Hash";

	private static final MySQLAccess		db						= new MySQLAccess();
	private final Parser<DiaryRecord>		parser					= new ParserDiaryRecord();
	private final Serializer<DiaryRecord>	serializer				= new SerializerAdapter<DiaryRecord>(parser);

	private final AuthService				authService				= new FrontendAuthService();

	protected int getCurrentUserId()
	{
		return authService.getCurrentUserId();
	}

	private List<Versioned<DiaryRecord>> parseDiaryRecords(ResultSet resultSet) throws SQLException
	{
		List<Versioned<DiaryRecord>> result = new LinkedList<Versioned<DiaryRecord>>();

		while (resultSet.next())
		{
			String id = resultSet.getString(COLUMN_DIARY_GUID);
			Date timeStamp = Utils.parseTimeUTC(resultSet.getString(COLUMN_DIARY_TIMESTAMP));
			String hash = resultSet.getString(COLUMN_DIARY_HASH);
			int version = resultSet.getInt(COLUMN_DIARY_VERSION);
			boolean deleted = (resultSet.getInt(COLUMN_DIARY_DELETED) == 1);
			String content = resultSet.getString(COLUMN_DIARY_CONTENT);

			Versioned<DiaryRecord> item = new Versioned<DiaryRecord>();
			item.setId(id.toLowerCase());
			item.setTimeStamp(timeStamp);
			item.setHash(hash);
			item.setVersion(version);
			item.setDeleted(deleted);
			item.setData(serializer.read(content));

			result.add(item);
		}

		return result;
	}

	@Override
	public void delete(String id)
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
		save(Arrays.asList(item));
	}

	@Override
	public Versioned<DiaryRecord> findById(String guid)
	{
		try
		{
			int userId = getCurrentUserId();
			String clause = String.format("(%s = %d) AND (%s = '%s')", COLUMN_DIARY_USER, userId, COLUMN_DIARY_GUID,
					guid);

			ResultSet set = db.select(TABLE_DIARY, clause, null);
			List<Versioned<DiaryRecord>> result = parseDiaryRecords(set);
			set.close();
			return result.isEmpty() ? null : result.get(0);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> findByIdPrefix(String prefix)
	{
		int userId = getCurrentUserId();

		if (prefix.length() != ObjectService.ID_PREFIX_SIZE)
		{
			throw new IllegalArgumentException(String.format("Invalid prefix length, expected %d chars, but %d found",
					ObjectService.ID_PREFIX_SIZE, prefix.length()));
		}

		try
		{
			String clause = String.format("(%s = %d) AND (%s LIKE '%s%%')", COLUMN_DIARY_USER, userId,
					COLUMN_DIARY_GUID, prefix);

			String order = COLUMN_DIARY_TIMECACHE;

			ResultSet set = db.select(TABLE_DIARY, clause, order);
			List<Versioned<DiaryRecord>> result = parseDiaryRecords(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date time)
	{
		try
		{
			int userId = getCurrentUserId();

			String clause = String.format("(%s = %d) AND (%s >= '%s')", COLUMN_DIARY_USER, userId,
					COLUMN_DIARY_TIMESTAMP, Utils.formatTimeUTC(time));

			String order = COLUMN_DIARY_TIMECACHE;

			ResultSet set = db.select(TABLE_DIARY, clause, order);
			List<Versioned<DiaryRecord>> result = parseDiaryRecords(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> findPeriod(Date startTime, Date endTime, boolean includeRemoved)
	{
		try
		{
			int userId = getCurrentUserId();

			String clause = String.format("(%s = %d) AND (%s >= '%s') AND (%s <= '%s')", COLUMN_DIARY_USER, userId,
					COLUMN_DIARY_TIMECACHE, Utils.formatTimeUTC(startTime), COLUMN_DIARY_TIMECACHE,
					Utils.formatTimeUTC(endTime));

			if (!includeRemoved)
			{
				clause += String.format(" AND (%s = '%s')", COLUMN_DIARY_DELETED, Utils.formatBooleanInt(false));
			}

			String order = COLUMN_DIARY_TIMECACHE;

			// System.out.println("Requesting SQL clause: " + clause);
			ResultSet set = db.select(TABLE_DIARY, clause, order);
			List<Versioned<DiaryRecord>> result = parseDiaryRecords(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public String getHash(String prefix)
	{
		try
		{
			int userId = getCurrentUserId();

			String clause = String.format("(%s = %d) AND (%s = '%s')", COLUMN_DIARY_HASH_USER, userId,
					COLUMN_DIARY_HASH_GUID, prefix);

			ResultSet set = db.select(TABLE_DIARY_HASH, clause, null);

			String hash = null;

			if (set.next())
			{
				hash = set.getString(COLUMN_DIARY_HASH_HASH);
			}

			set.close();
			return hash;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Map<String, String> getHashChildren(String prefix)
	{
		try
		{
			int userId = getCurrentUserId();

			String clause = String.format("(%s = %d) AND (%s LIKE '%s')", COLUMN_DIARY_HASH_USER, userId,
					COLUMN_DIARY_HASH_GUID, prefix + "_");
			ResultSet set = db.select(TABLE_DIARY_HASH, clause, null);

			Map<String, String> result = new HashMap<String, String>();
			while (set.next())
			{
				String id = set.getString(COLUMN_DIARY_HASH_GUID);
				String hash = set.getString(COLUMN_DIARY_HASH_HASH);
				result.put(id, hash);
			}

			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public void save(List<Versioned<DiaryRecord>> records)
	{
		try
		{
			int userId = getCurrentUserId();

			for (Versioned<DiaryRecord> item : records)
			{
				final String id = item.getId().toLowerCase();
				final String content = serializer.write(item.getData());
				final String timeCache = Utils.formatTimeUTC(item.getData().getTime());
				final String timeStamp = Utils.formatTimeUTC(item.getTimeStamp());
				final String hash = item.getHash();
				final String version = String.valueOf(item.getVersion());
				final String deleted = Utils.formatBooleanInt(item.isDeleted());

				// before persisting item
				updateHashTree(userId, id, hash);

				if (findById(item.getId()) != null)
				{
					// presented, update

					SortedMap<String, String> set = new TreeMap<String, String>();
					set.put(COLUMN_DIARY_TIMESTAMP, timeStamp);
					set.put(COLUMN_DIARY_HASH, hash);
					set.put(COLUMN_DIARY_VERSION, version);
					set.put(COLUMN_DIARY_DELETED, deleted);
					set.put(COLUMN_DIARY_CONTENT, content);
					set.put(COLUMN_DIARY_TIMECACHE, timeCache);

					SortedMap<String, String> where = new TreeMap<String, String>();
					where.put(COLUMN_DIARY_GUID, id);
					where.put(COLUMN_DIARY_USER, String.valueOf(userId));

					db.update(TABLE_DIARY, set, where);
				}
				else
				{
					// not presented, insert

					LinkedHashMap<String, String> set = new LinkedHashMap<String, String>();
					set.put(COLUMN_DIARY_GUID, id);
					set.put(COLUMN_DIARY_USER, String.valueOf(userId));
					set.put(COLUMN_DIARY_TIMESTAMP, timeStamp);
					set.put(COLUMN_DIARY_HASH, hash);
					set.put(COLUMN_DIARY_VERSION, version);
					set.put(COLUMN_DIARY_DELETED, deleted);
					set.put(COLUMN_DIARY_CONTENT, content);
					set.put(COLUMN_DIARY_TIMECACHE, timeCache);

					db.insert(TABLE_DIARY, set);
				}

			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@SuppressWarnings("static-method")
	private String getItemHash(int userId, String id)
	{
		try
		{
			String clause = String
					.format("(%s = %d) AND (%s = '%s')", COLUMN_DIARY_USER, userId, COLUMN_DIARY_GUID, id);

			ResultSet set = db.select(TABLE_DIARY, clause, null);

			String hash = null;

			if (set.next())
			{
				hash = set.getString(COLUMN_DIARY_HASH);
			}

			set.close();
			return hash;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	private void setHash(int userId, String prefix, String hash)
	{
		try
		{
			if (getHash(prefix) != null)
			{
				Map<String, String> set = new HashMap<String, String>();
				set.put(COLUMN_DIARY_HASH_HASH, hash);

				Map<String, String> where = new HashMap<String, String>();
				where.put(COLUMN_DIARY_HASH_USER, String.valueOf(userId));
				where.put(COLUMN_DIARY_HASH_GUID, prefix);

				db.update(TABLE_DIARY_HASH, set, where);
			}
			else
			{
				Map<String, String> set = new HashMap<String, String>();
				set.put(COLUMN_DIARY_HASH_USER, String.valueOf(userId));
				set.put(COLUMN_DIARY_HASH_GUID, prefix);
				set.put(COLUMN_DIARY_HASH_HASH, hash);

				db.insert(TABLE_DIARY_HASH, set);
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	private void updateHashTree(int userId, String id, String newItemHash)
	{
		String oldItemHash = getItemHash(userId, id);
		String hashDiff = Utils.subHash(newItemHash, oldItemHash);

		for (int i = 0; i <= ObjectService.ID_PREFIX_SIZE; i++)
		{
			String prefix = id.substring(0, i);
			String oldHash = getHash(prefix);
			String newHash = Utils.sumHash(oldHash, hashDiff);
			setHash(userId, prefix, newHash);
		}
	}
}
