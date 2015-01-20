package org.bosik.diacomp.web.backend.features.diary;

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
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.services.sync.HashUtils;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.mysql.MySQLAccess;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.springframework.beans.factory.annotation.Autowired;
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

	@Autowired
	private UserInfoService					userInfoService;

	protected int getCurrentUserId()
	{
		return userInfoService.getCurrentUserId();
	}

	private List<Versioned<DiaryRecord>> parseItems(ResultSet resultSet, int limit) throws SQLException
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

			if (limit > 0 && result.size() > limit)
			{
				throw new TooManyItemsException("Too many items");
			}
		}

		return result;
	}

	private List<Versioned<DiaryRecord>> parseItems(ResultSet resultSet) throws SQLException
	{
		return parseItems(resultSet, 0);
	}

	@Override
	public int count(String prefix)
	{
		int userId = getCurrentUserId();

		if (prefix == null)
		{
			throw new NullPointerException("ID prefix can't be null");
		}

		try
		{
			final String[] select = { "COUNT(*)" };
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = null;

			ResultSet set = db.select(TABLE_DIARY, select, where, whereArgs, order);

			if (set.next())
			{
				int count = set.getInt(1);
				set.close();
				return count;
			}
			else
			{
				throw new IllegalStateException("Failed to request SQL database");
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
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
	public Versioned<DiaryRecord> findById(String id)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
			final String[] whereArgs = { String.valueOf(userId), id };
			final String order = null;

			ResultSet set = db.select(TABLE_DIARY, select, where, whereArgs, order);

			List<Versioned<DiaryRecord>> result = parseItems(set);
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

		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = COLUMN_DIARY_TIMECACHE;

			ResultSet set = db.select(TABLE_DIARY, select, where, whereArgs, order);

			List<Versioned<DiaryRecord>> result = parseItems(set, MAX_ITEMS_COUNT);
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

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s >= ?)", COLUMN_DIARY_USER, COLUMN_DIARY_TIMESTAMP);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatTimeUTC(time) };
			final String order = COLUMN_DIARY_TIMECACHE;

			ResultSet set = db.select(TABLE_DIARY, select, where, whereArgs, order);

			List<Versioned<DiaryRecord>> result = parseItems(set);
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

			final String[] select = null; // all

			String where;
			String[] whereArgs;

			if (includeRemoved)
			{
				where = String.format("(%s = ?) AND (%s >= ?) AND (%s <= ?)", COLUMN_DIARY_USER,
						COLUMN_DIARY_TIMECACHE, COLUMN_DIARY_TIMECACHE);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatTimeUTC(startTime),
						Utils.formatTimeUTC(endTime) };
			}
			else
			{
				where = String.format("(%s = ?) AND (%s >= ?) AND (%s <= ?) AND (%s = ?)", COLUMN_DIARY_USER,
						COLUMN_DIARY_TIMECACHE, COLUMN_DIARY_TIMECACHE, COLUMN_DIARY_DELETED);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatTimeUTC(startTime),
						Utils.formatTimeUTC(endTime), Utils.formatBooleanInt(false) };
			}

			final String order = COLUMN_DIARY_TIMECACHE;

			// System.out.println("Requesting SQL clause: " + clause);
			ResultSet set = db.select(TABLE_DIARY, select, where, whereArgs, order);

			List<Versioned<DiaryRecord>> result = parseItems(set);
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

			final String[] select = { COLUMN_DIARY_HASH_HASH };
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DIARY_HASH_USER, COLUMN_DIARY_HASH_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix };
			final String order = null;

			ResultSet set = db.select(TABLE_DIARY_HASH, select, where, whereArgs, order);

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

			Map<String, String> result = new HashMap<String, String>();

			if (prefix.length() < ObjectService.ID_PREFIX_SIZE)
			{
				final String[] select = { COLUMN_DIARY_HASH_GUID, COLUMN_DIARY_HASH_HASH };
				final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DIARY_HASH_USER,
						COLUMN_DIARY_HASH_GUID);
				final String[] whereArgs = { String.valueOf(userId), prefix + "_" };
				final String order = null;

				ResultSet set = db.select(TABLE_DIARY_HASH, select, where, whereArgs, order);

				while (set.next())
				{
					String id = set.getString(COLUMN_DIARY_HASH_GUID);
					String hash = set.getString(COLUMN_DIARY_HASH_HASH);
					result.put(id, hash);
				}

				set.close();
			}
			else
			{
				final String[] select = { COLUMN_DIARY_GUID, COLUMN_DIARY_HASH };
				final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
				final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
				final String order = null;

				ResultSet set = db.select(TABLE_DIARY, select, where, whereArgs, order);

				while (set.next())
				{
					String id = set.getString(COLUMN_DIARY_GUID);
					String hash = set.getString(COLUMN_DIARY_HASH);
					result.put(id, hash);
				}

				set.close();
			}

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

				if (recordExists(userId, item.getId()))
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

	@Override
	public void setHash(String prefix, String hash)
	{
		try
		{
			int userId = getCurrentUserId();

			if (prefix.length() <= ObjectService.ID_PREFIX_SIZE)
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
			else if (prefix.length() == ObjectService.ID_FULL_SIZE)
			{
				if (recordExists(userId, prefix))
				{
					SortedMap<String, String> set = new TreeMap<String, String>();
					set.put(COLUMN_DIARY_HASH, hash);

					SortedMap<String, String> where = new TreeMap<String, String>();
					where.put(COLUMN_DIARY_USER, String.valueOf(userId));
					where.put(COLUMN_DIARY_GUID, prefix);

					db.update(TABLE_DIARY, set, where);
				}
				else
				{
					// fail
					throw new NotFoundException(prefix);
				}
			}
			else
			{
				throw new IllegalArgumentException(String.format(
						"Invalid prefix ('%s'), expected: 0..%d or %d chars, found: %d", prefix,
						ObjectService.ID_PREFIX_SIZE, ObjectService.ID_FULL_SIZE, prefix.length()));
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@SuppressWarnings("static-method")
	private String getDataHash(int userId, String id)
	{
		try
		{
			final String[] select = { COLUMN_DIARY_HASH };
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
			final String[] whereArgs = { String.valueOf(userId), id };
			final String order = null;

			ResultSet set = db.select(TABLE_DIARY, select, where, whereArgs, order);

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

	private void updateHashTree(int userId, String id, String newItemHash)
	{
		String oldItemHash = getDataHash(userId, id);
		String hashDiff = HashUtils.subHash(newItemHash, oldItemHash);

		for (int i = 0; i <= ObjectService.ID_PREFIX_SIZE; i++)
		{
			String prefix = id.substring(0, i);
			String oldHash = getHash(prefix);
			String newHash = HashUtils.sumHash(oldHash, hashDiff);
			setHash(prefix, newHash);
		}
	}

	@SuppressWarnings("static-method")
	private boolean recordExists(int userId, String id) throws SQLException
	{
		final String[] select = { COLUMN_DIARY_GUID };
		final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
		final String[] whereArgs = { String.valueOf(userId), id };
		final String order = null;

		ResultSet set = db.select(TABLE_DIARY, select, where, whereArgs, order);
		boolean result = set.first();

		set.close();
		return result;
	}
}
