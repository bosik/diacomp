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
import org.bosik.diacomp.core.services.sync.MemoryMerkleTree;
import org.bosik.diacomp.core.services.sync.MerkleTree;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;
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

	private final Parser<DiaryRecord>		parser					= new ParserDiaryRecord();
	private final Serializer<DiaryRecord>	serializer				= new SerializerAdapter<DiaryRecord>(parser);

	@Autowired
	private UserInfoService					userInfoService;

	protected int getCurrentUserId()
	{
		return userInfoService.getCurrentUserId();
	}

	List<Versioned<DiaryRecord>> parseItems(ResultSet resultSet, int limit) throws SQLException
	{
		if (limit > 0)
		{
			if (resultSet.last())
			{
				if (resultSet.getRow() > limit)
				{
					throw new TooManyItemsException("Too many items");
				}

				resultSet.beforeFirst();
			}
		}

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

	List<Versioned<DiaryRecord>> parseItems(ResultSet resultSet) throws SQLException
	{
		return parseItems(resultSet, 0);
	}

	@Override
	public int count(String prefix)
	{
		int userId = getCurrentUserId();

		if (prefix == null)
		{
			// TODO: switch to IllegalArgumentException("X is null") (here & everywhere)
			throw new NullPointerException("ID prefix can't be null");
		}

		try
		{
			final String[] select = { "COUNT(*)" };
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = null;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<Integer>()
			{
				@Override
				public Integer onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						return set.getInt(1);
					}
					else
					{
						throw new IllegalStateException("Failed to request SQL database");
					}
				}
			});

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
		int userId = getCurrentUserId();

		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
			final String[] whereArgs = { String.valueOf(userId), id };
			final String order = null;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order,
					new DataCallback<Versioned<DiaryRecord>>()
					{
						@Override
						public Versioned<DiaryRecord> onData(ResultSet set) throws SQLException
						{
							List<Versioned<DiaryRecord>> result = parseItems(set);
							return result.isEmpty() ? null : result.get(0);
						}
					});
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

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order,
					new DataCallback<List<Versioned<DiaryRecord>>>()
					{
						@Override
						public List<Versioned<DiaryRecord>> onData(ResultSet set) throws SQLException
						{
							return parseItems(set, MAX_ITEMS_COUNT);
						}
					});
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

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order,
					new DataCallback<List<Versioned<DiaryRecord>>>()
					{
						@Override
						public List<Versioned<DiaryRecord>> onData(ResultSet set) throws SQLException
						{
							return parseItems(set, MAX_ITEMS_COUNT);
						}
					});
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
				where = String.format("(%s = ?) AND (%s >= ?) AND (%s < ?)", COLUMN_DIARY_USER, COLUMN_DIARY_TIMECACHE,
						COLUMN_DIARY_TIMECACHE);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatTimeUTC(startTime),
						Utils.formatTimeUTC(endTime) };
			}
			else
			{
				where = String.format("(%s = ?) AND (%s >= ?) AND (%s < ?) AND (%s = ?)", COLUMN_DIARY_USER,
						COLUMN_DIARY_TIMECACHE, COLUMN_DIARY_TIMECACHE, COLUMN_DIARY_DELETED);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatTimeUTC(startTime),
						Utils.formatTimeUTC(endTime), Utils.formatBooleanInt(false) };
			}

			final String order = COLUMN_DIARY_TIMECACHE;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order,
					new DataCallback<List<Versioned<DiaryRecord>>>()
					{
						@Override
						public List<Versioned<DiaryRecord>> onData(ResultSet set) throws SQLException
						{
							return parseItems(set, MAX_ITEMS_COUNT);
						}
					});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public String getHash(String prefix)
	{
		int userId = getCurrentUserId();

		try
		{
			final String[] select = { COLUMN_DIARY_HASH_HASH };
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DIARY_HASH_USER, COLUMN_DIARY_HASH_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix };
			final String order = null;

			return MySQLAccess.select(TABLE_DIARY_HASH, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						return set.getString(COLUMN_DIARY_HASH_HASH);
					}
					else
					{
						return null;
					}
				}
			});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Map<String, String> getHashChildren(String prefix)
	{
		int userId = getCurrentUserId();

		try
		{
			String[] select;
			String where;
			String[] whereArgs;
			String order = null;

			if (prefix.length() < ObjectService.ID_PREFIX_SIZE)
			{
				select = new String[] { COLUMN_DIARY_HASH_GUID, COLUMN_DIARY_HASH_HASH };
				where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DIARY_HASH_USER, COLUMN_DIARY_HASH_GUID);
				whereArgs = new String[] { String.valueOf(userId), prefix + "_" };
			}
			else
			{
				select = new String[] { COLUMN_DIARY_GUID, COLUMN_DIARY_HASH };
				where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
				whereArgs = new String[] { String.valueOf(userId), prefix + "%" };
			}

			return MySQLAccess.select(TABLE_DIARY_HASH, select, where, whereArgs, order,
					new DataCallback<Map<String, String>>()
					{
						@Override
						public Map<String, String> onData(ResultSet set) throws SQLException
						{
							Map<String, String> result = new HashMap<String, String>();

							while (set.next())
							{
								String id = set.getString(COLUMN_DIARY_GUID);
								String hash = set.getString(COLUMN_DIARY_HASH);
								result.put(id, hash);
							}

							return result;
						}
					});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public MerkleTree getHashTree()
	{
		int userId = getCurrentUserId();

		SortedMap<String, String> hashes = getDataHashes(userId);
		SortedMap<String, String> tree = HashUtils.buildHashTree(hashes);

		MemoryMerkleTree result = new MemoryMerkleTree();
		result.putAll(tree); // headers (0..4 chars id)
		result.putAll(hashes); // leafs (32 chars id)
		return result;
	}

	@Override
	public void save(List<Versioned<DiaryRecord>> records)
	{
		int userId = getCurrentUserId();

		try
		{

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

					MySQLAccess.update(TABLE_DIARY, set, where);
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

					MySQLAccess.insert(TABLE_DIARY, set);
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
		int userId = getCurrentUserId();

		try
		{
			if (prefix.length() <= ObjectService.ID_PREFIX_SIZE)
			{
				if (getHash(prefix) != null)
				{
					Map<String, String> set = new HashMap<String, String>();
					set.put(COLUMN_DIARY_HASH_HASH, hash);

					Map<String, String> where = new HashMap<String, String>();
					where.put(COLUMN_DIARY_HASH_USER, String.valueOf(userId));
					where.put(COLUMN_DIARY_HASH_GUID, prefix);

					MySQLAccess.update(TABLE_DIARY_HASH, set, where);
				}
				else
				{
					Map<String, String> set = new HashMap<String, String>();
					set.put(COLUMN_DIARY_HASH_USER, String.valueOf(userId));
					set.put(COLUMN_DIARY_HASH_GUID, prefix);
					set.put(COLUMN_DIARY_HASH_HASH, hash);

					MySQLAccess.insert(TABLE_DIARY_HASH, set);
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

					MySQLAccess.update(TABLE_DIARY, set, where);
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

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						return set.getString(COLUMN_DIARY_HASH);
					}
					else
					{
						return null;
					}
				}
			});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	/**
	 * Returns sorted map (ID, Hash) for all items
	 * 
	 * @return
	 */
	@SuppressWarnings("static-method")
	private SortedMap<String, String> getDataHashes(int userId)
	{
		try
		{
			final String[] select = { COLUMN_DIARY_GUID, COLUMN_DIARY_HASH };
			final String where = String.format("(%s = ?)", COLUMN_DIARY_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order,
					new DataCallback<SortedMap<String, String>>()
					{
						@Override
						public SortedMap<String, String> onData(ResultSet set) throws SQLException
						{
							SortedMap<String, String> result = new TreeMap<String, String>();

							while (set.next())
							{
								String id = set.getString(COLUMN_DIARY_GUID);
								String hash = set.getString(COLUMN_DIARY_HASH);
								// THINK: probably storing entries is unnecessary, so we should process it as we go
								result.put(id, hash);
							}

							return result;
						}
					});
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

		return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<Boolean>()
		{
			@Override
			public Boolean onData(ResultSet set) throws SQLException
			{
				return set.first();
			}
		});
	}
}