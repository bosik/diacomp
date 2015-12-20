/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.diary;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.CachedHashTree;
import org.bosik.diacomp.web.backend.common.CachedHashTree.TreeType;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.merklesync.DataSource;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MemoryMerkleTree;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("real")
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

	private static final int				MAX_READ_ITEMS			= 500;

	private final Parser<DiaryRecord>		parser					= new ParserDiaryRecord();
	private final Serializer<DiaryRecord>	serializer				= new SerializerAdapter<DiaryRecord>(parser);

	@Autowired
	private UserInfoService					userInfoService;

	@Autowired
	private CachedHashTree					cachedHashTree;

	private int getCurrentUserId()
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

		List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();

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
			throw new IllegalArgumentException("ID prefix is null");
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
							return parseItems(set, MAX_READ_ITEMS);
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
							return parseItems(set, 0);
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
							return parseItems(set, MAX_READ_ITEMS);
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

		MerkleTree tree = cachedHashTree.getTree(userId, TreeType.DIARY);
		if (tree == null)
		{
			tree = rebuildHashTree(userId);
			cachedHashTree.setTree(userId, TreeType.DIARY, tree);
		}
		else
		{
			///**/System.out.println("Returning cached hash tree");
		}

		return tree;
	}

	private MerkleTree rebuildHashTree(int userId)
	{
		MemoryMerkleTree result = new MemoryMerkleTree();
		/**/long timeStart = System.currentTimeMillis();

		SortedMap<String, String> hashes = getDataHashes(userId);
		/**/long timeFetch = System.currentTimeMillis();

		SortedMap<String, String> tree = HashUtils.buildHashTree(hashes);
		/**/long timeProcess = System.currentTimeMillis();

		result = new MemoryMerkleTree();
		result.putAll(tree); // headers (0..4 chars id)
		result.putAll(hashes);
		// leafs (32 chars id)
		/**/long timePut = System.currentTimeMillis();
		/**/System.out.println(String.format("Tree built in %s ms (fetch: %d ms, process: %d ms, put: %d ms)",
				System.currentTimeMillis() - timeStart, timeFetch - timeStart, timeProcess - timeFetch,
				timePut - timeProcess));
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

				if (id.length() < DataSource.ID_FULL_SIZE)
				{
					throw new IllegalArgumentException(
							String.format("Invalid ID: %s, must be %d characters long", id, DataSource.ID_FULL_SIZE));
				}

				final String content = serializer.write(item.getData());
				final String timeCache = Utils.formatTimeUTC(item.getData().getTime());
				final String timeStamp = Utils.formatTimeUTC(item.getTimeStamp());
				final String hash = item.getHash();
				final String version = String.valueOf(item.getVersion());
				final String deleted = Utils.formatBooleanInt(item.isDeleted());

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

				cachedHashTree.setTree(userId, TreeType.DIARY, null);
			}
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
