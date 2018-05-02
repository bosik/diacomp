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
package org.bosik.diacomp.web.backend.features.base.dish;

import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.utils.Utils;
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
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;

@Service
// @Profile("real")
public class DishBaseLocalService
{
	// Dishbase table
	private static final String TABLE_DISHBASE            = "dishbase2";
	private static final String COLUMN_DISHBASE_GUID      = "_GUID";
	private static final String COLUMN_DISHBASE_USER      = "_UserID";
	private static final String COLUMN_DISHBASE_TIMESTAMP = "_TimeStamp";
	private static final String COLUMN_DISHBASE_HASH      = "_Hash";
	private static final String COLUMN_DISHBASE_VERSION   = "_Version";
	private static final String COLUMN_DISHBASE_DELETED   = "_Deleted";
	private static final String COLUMN_DISHBASE_CONTENT   = "_Content";
	private static final String COLUMN_DISHBASE_NAMECACHE = "_NameCache";

	private static final int MAX_READ_ITEMS = 500;

	private static final Parser<DishItem>     parser     = new ParserDishItem();
	private static final Serializer<DishItem> serializer = new SerializerAdapter<>(parser);

	@Autowired
	private CachedDishHashTree cachedHashTree;

	private static List<Versioned<DishItem>> parseItems(ResultSet resultSet, int limit) throws SQLException
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

		List<Versioned<DishItem>> result = new ArrayList<>();

		while (resultSet.next())
		{
			String id = resultSet.getString(COLUMN_DISHBASE_GUID);
			Date timeStamp = Utils.parseTimeUTC(resultSet.getString(COLUMN_DISHBASE_TIMESTAMP));
			String hash = resultSet.getString(COLUMN_DISHBASE_HASH);
			int version = resultSet.getInt(COLUMN_DISHBASE_VERSION);
			boolean deleted = (resultSet.getInt(COLUMN_DISHBASE_DELETED) == 1);
			String content = resultSet.getString(COLUMN_DISHBASE_CONTENT);

			Versioned<DishItem> item = new Versioned<>();
			item.setId(id);
			item.setTimeStamp(timeStamp);
			item.setHash(hash);
			item.setVersion(version);
			item.setDeleted(deleted);
			item.setData(serializer.read(content));

			result.add(item);
		}

		return result;
	}

	private static List<Versioned<DishItem>> parseItems(ResultSet resultSet) throws SQLException
	{
		return parseItems(resultSet, 0);
	}

	public void add(int userId, Versioned<DishItem> item) throws DuplicateException, PersistenceException
	{
		save(userId, Collections.singletonList(item));
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
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<Integer>()
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
		Versioned<DishItem> item = findById(userId, id);

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

	public List<Versioned<DishItem>> findAll(int userId, boolean includeRemoved)
	{
		try
		{
			final String[] select = null; // all
			String where;
			String[] whereArgs;

			if (includeRemoved)
			{
				where = String.format("(%s = ?)", COLUMN_DISHBASE_USER);
				whereArgs = new String[] { String.valueOf(userId) };
			}
			else
			{
				where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_DELETED);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatBooleanInt(false) };
			}

			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<List<Versioned<DishItem>>>()
			{
				@Override
				public List<Versioned<DishItem>> onData(ResultSet set) throws SQLException
				{
					return parseItems(set);
				}
			});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	public List<Versioned<DishItem>> findAny(int userId, String filter)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?) AND (%s LIKE ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_DELETED,
					COLUMN_DISHBASE_NAMECACHE);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), "%" + filter + "%" };
			final String order = COLUMN_DISHBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<List<Versioned<DishItem>>>()
			{
				@Override
				public List<Versioned<DishItem>> onData(ResultSet set) throws SQLException
				{
					return parseItems(set);
				}
			});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	public Versioned<DishItem> findById(int userId, String id)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), id };
			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<Versioned<DishItem>>()
			{
				@Override
				public Versioned<DishItem> onData(ResultSet set) throws SQLException
				{
					List<Versioned<DishItem>> result = parseItems(set);
					return result.isEmpty() ? null : result.get(0);
				}
			});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	public List<Versioned<DishItem>> findByIdPrefix(int userId, String prefix)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = COLUMN_DISHBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<List<Versioned<DishItem>>>()
			{
				@Override
				public List<Versioned<DishItem>> onData(ResultSet set) throws SQLException
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

	public List<Versioned<DishItem>> findChanged(int userId, Date since)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s >= ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_TIMESTAMP);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatTimeUTC(since) };
			final String order = COLUMN_DISHBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<List<Versioned<DishItem>>>()
			{
				@Override
				public List<Versioned<DishItem>> onData(ResultSet set) throws SQLException
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

	public Versioned<DishItem> findOne(int userId, String exactName)
	{
		try
		{
			final String[] select = null; // all
			final String where = String
					.format("(%s = ?) AND (%s = ?) AND (%s = ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_DELETED, COLUMN_DISHBASE_NAMECACHE);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), exactName };
			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<Versioned<DishItem>>()
			{
				@Override
				public Versioned<DishItem> onData(ResultSet set) throws SQLException
				{
					List<Versioned<DishItem>> result = parseItems(set);
					return result.isEmpty() ? null : result.get(0);
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
			final String[] select = { COLUMN_DISHBASE_GUID, COLUMN_DISHBASE_HASH };
			final String where = String.format("(%s = ?)", COLUMN_DISHBASE_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<SortedMap<String, String>>()
			{
				@Override
				public SortedMap<String, String> onData(ResultSet set) throws SQLException
				{
					SortedMap<String, String> result = new TreeMap<String, String>();

					while (set.next())
					{
						String id = set.getString(COLUMN_DISHBASE_GUID);
						String hash = set.getString(COLUMN_DISHBASE_HASH);
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

	public MerkleTree getHashTree(int userId)
	{
		MerkleTree tree = cachedHashTree.get(userId);
		if (tree == null)
		{
			tree = HashUtils.buildMerkleTree(getDataHashes(userId));
			cachedHashTree.set(userId, tree);
		}

		return tree;
	}

	public void save(int userId, List<Versioned<DishItem>> items)
	{
		try
		{
			for (Versioned<DishItem> item : items)
			{
				if (item.getId() == null || item.getId().length() < ObjectService.ID_FULL_SIZE)
				{
					throw new IllegalArgumentException(String.format(Locale.US, "Invalid ID: %s, must be %d characters long", item.getId(),
							ObjectService.ID_FULL_SIZE));
				}

				Map<String, String> set = new HashMap<>();

				set.put(COLUMN_DISHBASE_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
				set.put(COLUMN_DISHBASE_HASH, item.getHash());
				set.put(COLUMN_DISHBASE_VERSION, String.valueOf(item.getVersion()));
				set.put(COLUMN_DISHBASE_DELETED, Utils.formatBooleanInt(item.isDeleted()));
				set.put(COLUMN_DISHBASE_CONTENT, serializer.write(item.getData()));
				set.put(COLUMN_DISHBASE_NAMECACHE, item.getData().getName());

				if (recordExists(userId, item.getId()))
				{
					// presented, update

					Map<String, String> where = new HashMap<>();
					where.put(COLUMN_DISHBASE_GUID, item.getId());
					where.put(COLUMN_DISHBASE_USER, String.valueOf(userId));

					MySQLAccess.update(TABLE_DISHBASE, set, where);
				}
				else
				{
					// not presented, insert

					set.put(COLUMN_DISHBASE_GUID, item.getId());
					set.put(COLUMN_DISHBASE_USER, String.valueOf(userId));

					MySQLAccess.insert(TABLE_DISHBASE, set);
				}

				cachedHashTree.set(userId, null);
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@SuppressWarnings("static-method")
	private boolean recordExists(int userId, String id) throws SQLException
	{
		final String[] select = { COLUMN_DISHBASE_GUID };
		final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_GUID);
		final String[] whereArgs = { String.valueOf(userId), id };
		final String order = null;

		return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<Boolean>()
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
			final String where = String.format("(%s = ?)", COLUMN_DISHBASE_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_DISHBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();
					s.append("[");

					while (resultSet.next())
					{
						String id = resultSet.getString(COLUMN_DISHBASE_GUID);
						String timeStamp = Utils.formatTimeLocal(TimeZone.getDefault(), resultSet.getTimestamp(COLUMN_DISHBASE_TIMESTAMP));
						String hash = resultSet.getString(COLUMN_DISHBASE_HASH);
						int version = resultSet.getInt(COLUMN_DISHBASE_VERSION);
						boolean deleted = (resultSet.getInt(COLUMN_DISHBASE_DELETED) == 1);
						String content = resultSet.getString(COLUMN_DISHBASE_CONTENT);

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
			final String where = String.format("(%s = ?)", COLUMN_DISHBASE_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_DISHBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();

					s.append("VERSION=1\n");
					while (resultSet.next())
					{
						String name = resultSet.getString(COLUMN_DISHBASE_NAMECACHE);
						String id = resultSet.getString(COLUMN_DISHBASE_GUID);
						String timeStamp = Utils.formatTimeUTC(resultSet.getTimestamp(COLUMN_DISHBASE_TIMESTAMP));
						String hash = resultSet.getString(COLUMN_DISHBASE_HASH);
						int version = resultSet.getInt(COLUMN_DISHBASE_VERSION);
						boolean deleted = (resultSet.getInt(COLUMN_DISHBASE_DELETED) == 1);
						String content = resultSet.getString(COLUMN_DISHBASE_CONTENT);

						s.append(Utils.removeTabs(name)).append('\t');
						s.append(id).append('\t');
						s.append(timeStamp).append('\t');
						s.append(hash).append('\t');
						s.append(version).append('\t');
						s.append(deleted ? "true" : "false").append('\t');
						s.append(content).append('\n');
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

			MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					while (resultSet.next())
					{
						String id = resultSet.getString(COLUMN_DISHBASE_GUID);
						String userId = resultSet.getString(COLUMN_DISHBASE_USER);
						String content = resultSet.getString(COLUMN_DISHBASE_CONTENT);

						try
						{
							new JSONObject(content);
						}
						catch (Exception e)
						{
							System.out.println(id + "\t" + userId + "\t" + content);
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
