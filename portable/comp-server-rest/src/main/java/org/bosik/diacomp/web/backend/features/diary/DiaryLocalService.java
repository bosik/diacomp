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

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.CachedHashTree;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;

@Service
// @Profile("real")
public class DiaryLocalService
{
	// Diary table
	private static final String TABLE_DIARY            = "diary2";
	private static final String COLUMN_DIARY_USER      = "_UserID";
	private static final String COLUMN_DIARY_GUID      = "_GUID";
	private static final String COLUMN_DIARY_TIMESTAMP = "_TimeStamp";
	private static final String COLUMN_DIARY_HASH      = "_Hash";
	private static final String COLUMN_DIARY_VERSION   = "_Version";
	private static final String COLUMN_DIARY_DELETED   = "_Deleted";
	private static final String COLUMN_DIARY_CONTENT   = "_Content";
	private static final String COLUMN_DIARY_TIMECACHE = "_TimeCache";

	private static final int MAX_READ_ITEMS = 0;

	private final Parser<DiaryRecord>     parser     = new ParserDiaryRecord();
	private final Serializer<DiaryRecord> serializer = new SerializerAdapter<>(parser);

	@Autowired
	private CachedHashTree cachedHashTree;

	private List<Versioned<DiaryRecord>> parseItems(ResultSet resultSet, int limit) throws SQLException
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

		List<Versioned<DiaryRecord>> result = new ArrayList<>();

		while (resultSet.next())
		{
			String id = resultSet.getString(COLUMN_DIARY_GUID);
			Date timeStamp = Utils.parseTimeUTC(resultSet.getString(COLUMN_DIARY_TIMESTAMP));
			String hash = resultSet.getString(COLUMN_DIARY_HASH);
			int version = resultSet.getInt(COLUMN_DIARY_VERSION);
			boolean deleted = (resultSet.getInt(COLUMN_DIARY_DELETED) == 1);
			String content = resultSet.getString(COLUMN_DIARY_CONTENT);

			Versioned<DiaryRecord> item = new Versioned<>();
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

	private List<Versioned<DiaryRecord>> parseItems(ResultSet resultSet) throws SQLException
	{
		return parseItems(resultSet, 0);
	}

	private static boolean verify(Versioned<DiaryRecord> record)
	{
		if (record != null && record.getId() != null && record.getId().length() == ObjectService.ID_FULL_SIZE)
		{
			record.setId(record.getId().toLowerCase());
			return true;
		}
		else
		{
			return false;
		}
	}

	public void add(int userId, Versioned<DiaryRecord> item) throws DuplicateException, PersistenceException
	{
		try
		{
			if (!verify(item))
			{
				throw new IllegalArgumentException("Invalid record: " + item);
			}

			if (!recordExists(userId, item.getId()))
			{
				insert(userId, item);
			}
			else
			{
				throw new DuplicateException(item.getId());
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	public int count(int userId, String prefix)
	{
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

	public void delete(int userId, String id)
	{
		Versioned<DiaryRecord> item = findById(userId, id);

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
		save(userId, Collections.singletonList(item));
	}

	public Versioned<DiaryRecord> findById(int userId, String id)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
			final String[] whereArgs = { String.valueOf(userId), id };
			final String order = null;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<Versioned<DiaryRecord>>()
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

	public List<Versioned<DiaryRecord>> findByIdPrefix(int userId, String prefix)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DIARY_USER, COLUMN_DIARY_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = COLUMN_DIARY_TIMECACHE;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<List<Versioned<DiaryRecord>>>()
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

	public List<Versioned<DiaryRecord>> findChanged(int userId, Date time)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s >= ?)", COLUMN_DIARY_USER, COLUMN_DIARY_TIMESTAMP);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatTimeUTC(time) };
			final String order = COLUMN_DIARY_TIMECACHE;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<List<Versioned<DiaryRecord>>>()
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

	public List<Versioned<DiaryRecord>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved)
	{
		try
		{
			final String[] select = null; // all

			String where;
			String[] whereArgs;

			if (includeRemoved)
			{
				where = String
						.format("(%s = ?) AND (%s >= ?) AND (%s < ?)", COLUMN_DIARY_USER, COLUMN_DIARY_TIMECACHE, COLUMN_DIARY_TIMECACHE);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime) };
			}
			else
			{
				where = String.format("(%s = ?) AND (%s >= ?) AND (%s < ?) AND (%s = ?)", COLUMN_DIARY_USER, COLUMN_DIARY_TIMECACHE,
						COLUMN_DIARY_TIMECACHE, COLUMN_DIARY_DELETED);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatTimeUTC(startTime), Utils.formatTimeUTC(endTime),
						Utils.formatBooleanInt(false) };
			}

			final String order = COLUMN_DIARY_TIMECACHE;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<List<Versioned<DiaryRecord>>>()
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

	public MerkleTree getHashTree(int userId)
	{
		MerkleTree tree = cachedHashTree.getDiaryTree(userId);
		if (tree == null)
		{
			tree = HashUtils.buildMerkleTree(getDataHashes(userId));
			cachedHashTree.setDiaryTree(userId, tree);
		}

		return tree;
	}

	public void save(int userId, List<Versioned<DiaryRecord>> records)
	{
		try
		{
			for (Versioned<DiaryRecord> item : records)
			{
				if (!verify(item))
				{
					throw new IllegalArgumentException("Invalid record: " + item);
				}

				if (recordExists(userId, item.getId()))
				{
					update(userId, item);
				}
				else
				{
					insert(userId, item);
				}

			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	private void insert(int userId, Versioned<DiaryRecord> item) throws SQLException
	{
		LinkedHashMap<String, String> set = new LinkedHashMap<>();
		set.put(COLUMN_DIARY_GUID, item.getId());
		set.put(COLUMN_DIARY_USER, String.valueOf(userId));
		set.put(COLUMN_DIARY_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
		set.put(COLUMN_DIARY_HASH, item.getHash());
		set.put(COLUMN_DIARY_VERSION, String.valueOf(item.getVersion()));
		set.put(COLUMN_DIARY_DELETED, Utils.formatBooleanInt(item.isDeleted()));
		set.put(COLUMN_DIARY_CONTENT, serializer.write(item.getData()));
		set.put(COLUMN_DIARY_TIMECACHE, Utils.formatTimeUTC(item.getData().getTime()));

		MySQLAccess.insert(TABLE_DIARY, set);

		cachedHashTree.setDiaryTree(userId, null);
	}

	private void update(int userId, Versioned<DiaryRecord> item) throws SQLException
	{
		SortedMap<String, String> set = new TreeMap<>();
		set.put(COLUMN_DIARY_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
		set.put(COLUMN_DIARY_HASH, item.getHash());
		set.put(COLUMN_DIARY_VERSION, String.valueOf(item.getVersion()));
		set.put(COLUMN_DIARY_DELETED, Utils.formatBooleanInt(item.isDeleted()));
		set.put(COLUMN_DIARY_CONTENT, serializer.write(item.getData()));
		set.put(COLUMN_DIARY_TIMECACHE, Utils.formatTimeUTC(item.getData().getTime()));

		SortedMap<String, String> where = new TreeMap<>();
		where.put(COLUMN_DIARY_GUID, item.getId());
		where.put(COLUMN_DIARY_USER, String.valueOf(userId));

		MySQLAccess.update(TABLE_DIARY, set, where);

		cachedHashTree.setDiaryTree(userId, null);
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

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<SortedMap<String, String>>()
			{
				@Override
				public SortedMap<String, String> onData(ResultSet set) throws SQLException
				{
					SortedMap<String, String> result = new TreeMap<String, String>();

					while (set.next())
					{
						String id = set.getString(COLUMN_DIARY_GUID);
						String hash = set.getString(COLUMN_DIARY_HASH);
						// THINK: probably storing entries is unnecessary, so we should
						// process it as we go
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

	public String exportData(int userId)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?)", COLUMN_DIARY_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_DIARY_TIMECACHE;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();
					s.append("[");

					while (resultSet.next())
					{
						String id = resultSet.getString(COLUMN_DIARY_GUID);
						String timeStamp = Utils.formatTimeUTC(resultSet.getTimestamp(COLUMN_DIARY_TIMESTAMP));
						String hash = resultSet.getString(COLUMN_DIARY_HASH);
						int version = resultSet.getInt(COLUMN_DIARY_VERSION);
						boolean deleted = (resultSet.getInt(COLUMN_DIARY_DELETED) == 1);
						String content = resultSet.getString(COLUMN_DIARY_CONTENT);

						if (!resultSet.isFirst())
						{
							s.append(",");
						}

						s.append("{");
						s.append("\"id\":\"").append(id).append("\",");
						s.append("\"stamp\":\"").append(timeStamp).append("\",");
						s.append("\"hash\":\"").append(hash).append("\",");
						s.append("\"version\":").append(version).append(",");
						s.append("\"deleted\":").append(deleted).append(",");
						s.append("\"data\":").append(content);
						s.append("}");
					}

					s.append("]");
					return s.toString();
				}
			});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	public String exportPlain(int userId)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?)", COLUMN_DIARY_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_DIARY_TIMECACHE;

			return MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();

					s.append("VERSION=6\n");
					while (resultSet.next())
					{
						String id = resultSet.getString(COLUMN_DIARY_GUID);
						String timeStamp = Utils.formatTimeLocal(TimeZone.getDefault(), resultSet.getTimestamp(COLUMN_DIARY_TIMESTAMP));
						String time = Utils.formatTimeLocal(TimeZone.getDefault(), resultSet.getTimestamp(COLUMN_DIARY_TIMECACHE));
						String hash = resultSet.getString(COLUMN_DIARY_HASH);
						int version = resultSet.getInt(COLUMN_DIARY_VERSION);
						boolean deleted = (resultSet.getInt(COLUMN_DIARY_DELETED) == 1);
						String content = resultSet.getString(COLUMN_DIARY_CONTENT);

						s.append(time).append('\t');
						s.append(id).append('\t');
						s.append(timeStamp).append('\t');
						s.append(hash).append('\t');
						s.append(version).append('\t');
						s.append(deleted ? "true" : "false").append('\t');
						s.append(Utils.removeTabs(content)).append('\n');
					}

					return s.toString();
				}
			});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	public void validate()
	{
		try
		{
			final String[] select = null; // all
			final String where = "1=1";
			final String[] whereArgs = {};
			final String order = null;

			MySQLAccess.select(TABLE_DIARY, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					while (resultSet.next())
					{
						String id = resultSet.getString(COLUMN_DIARY_GUID);
						String userId = resultSet.getString(COLUMN_DIARY_USER);
						String time = Utils.formatTimeLocal(TimeZone.getDefault(), resultSet.getTimestamp(COLUMN_DIARY_TIMECACHE));
						String content = resultSet.getString(COLUMN_DIARY_CONTENT);

						try
						{
							new JSONObject(content);
						}
						catch (Exception e)
						{
							System.out.println(id + "\t" + userId + "\t" + time + "\t" + content);
						}
					}

					return null;
				}
			});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}
}
