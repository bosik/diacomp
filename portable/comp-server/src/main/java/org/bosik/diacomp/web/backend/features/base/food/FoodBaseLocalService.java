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
package org.bosik.diacomp.web.backend.features.base.food;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.services.transfer.Exportable;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.CachedHashTree;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
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
import java.util.TreeMap;

@Service
// @Profile("real")
public class FoodBaseLocalService implements FoodBaseService, Exportable
{
	// Foodbase table
	private static final String TABLE_FOODBASE            = "foodbase2";
	private static final String COLUMN_FOODBASE_GUID      = "_GUID";
	private static final String COLUMN_FOODBASE_USER      = "_UserID";
	private static final String COLUMN_FOODBASE_TIMESTAMP = "_TimeStamp";
	private static final String COLUMN_FOODBASE_HASH      = "_Hash";
	private static final String COLUMN_FOODBASE_VERSION   = "_Version";
	private static final String COLUMN_FOODBASE_DELETED   = "_Deleted";
	private static final String COLUMN_FOODBASE_CONTENT   = "_Content";
	private static final String COLUMN_FOODBASE_NAMECACHE = "_NameCache";

	private static final int MAX_READ_ITEMS = 500;

	private static final Parser<FoodItem>     parser     = new ParserFoodItem();
	private static final Serializer<FoodItem> serializer = new SerializerAdapter<>(parser);

	@Autowired
	private UserInfoService userInfoService;

	@Autowired
	private CachedHashTree cachedHashTree;

	private int getCurrentUserId()
	{
		return userInfoService.getCurrentUserId();
	}

	private static List<Versioned<FoodItem>> parseItems(ResultSet resultSet, int limit) throws SQLException
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

		List<Versioned<FoodItem>> result = new ArrayList<>();

		while (resultSet.next())
		{
			String id = resultSet.getString(COLUMN_FOODBASE_GUID);
			Date timeStamp = Utils.parseTimeUTC(resultSet.getString(COLUMN_FOODBASE_TIMESTAMP));
			String hash = resultSet.getString(COLUMN_FOODBASE_HASH);
			int version = resultSet.getInt(COLUMN_FOODBASE_VERSION);
			boolean deleted = (resultSet.getInt(COLUMN_FOODBASE_DELETED) == 1);
			String content = resultSet.getString(COLUMN_FOODBASE_CONTENT);

			Versioned<FoodItem> item = new Versioned<>();
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

	private static List<Versioned<FoodItem>> parseItems(ResultSet resultSet) throws SQLException
	{
		return parseItems(resultSet, 0);
	}

	@Override
	public void add(Versioned<FoodItem> item) throws DuplicateException, PersistenceException
	{
		save(Collections.singletonList(item));
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
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = null;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<Integer>()
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
		Versioned<FoodItem> item = findById(id);

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
		save(Collections.singletonList(item));
	}

	@Override
	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			String where;
			String[] whereArgs;

			if (includeRemoved)
			{
				where = String.format("(%s = ?)", COLUMN_FOODBASE_USER);
				whereArgs = new String[] { String.valueOf(userId) };
			}
			else
			{
				where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_DELETED);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatBooleanInt(false) };
			}

			final String order = null;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<List<Versioned<FoodItem>>>()
			{
				@Override
				public List<Versioned<FoodItem>> onData(ResultSet set) throws SQLException
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

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?) AND (%s LIKE ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_DELETED,
					COLUMN_FOODBASE_NAMECACHE);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), "%" + filter + "%" };
			final String order = COLUMN_FOODBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<List<Versioned<FoodItem>>>()
			{
				@Override
				public List<Versioned<FoodItem>> onData(ResultSet set) throws SQLException
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

	@Override
	public Versioned<FoodItem> findById(String id)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), id };
			final String order = null;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<Versioned<FoodItem>>()
			{
				@Override
				public Versioned<FoodItem> onData(ResultSet set) throws SQLException
				{
					List<Versioned<FoodItem>> result = parseItems(set);
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
	public List<Versioned<FoodItem>> findByIdPrefix(String prefix)
	{
		int userId = getCurrentUserId();

		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = COLUMN_FOODBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<List<Versioned<FoodItem>>>()
			{
				@Override
				public List<Versioned<FoodItem>> onData(ResultSet set) throws SQLException
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
	public List<Versioned<FoodItem>> findChanged(Date since)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s >= ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_TIMESTAMP);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatTimeUTC(since) };
			final String order = COLUMN_FOODBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<List<Versioned<FoodItem>>>()
			{
				@Override
				public List<Versioned<FoodItem>> onData(ResultSet set) throws SQLException
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
	public Versioned<FoodItem> findOne(String exactName)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String
					.format("(%s = ?) AND (%s = ?) AND (%s = ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_DELETED, COLUMN_FOODBASE_NAMECACHE);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), exactName };
			final String order = null;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<Versioned<FoodItem>>()
			{
				@Override
				public Versioned<FoodItem> onData(ResultSet set) throws SQLException
				{
					List<Versioned<FoodItem>> result = parseItems(set);
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
			final String[] select = { COLUMN_FOODBASE_GUID, COLUMN_FOODBASE_HASH };
			final String where = String.format("(%s = ?)", COLUMN_FOODBASE_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<SortedMap<String, String>>()
			{
				@Override
				public SortedMap<String, String> onData(ResultSet set) throws SQLException
				{
					SortedMap<String, String> result = new TreeMap<String, String>();

					while (set.next())
					{
						String id = set.getString(COLUMN_FOODBASE_GUID);
						String hash = set.getString(COLUMN_FOODBASE_HASH);
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

	@Override
	public MerkleTree getHashTree()
	{
		int userId = getCurrentUserId();

		MerkleTree tree = cachedHashTree.getFoodTree(userId);
		if (tree == null)
		{
			tree = HashUtils.buildMerkleTree(getDataHashes(userId));
			cachedHashTree.setFoodTree(userId, tree);
		}

		return tree;
	}

	@Override
	public void save(List<Versioned<FoodItem>> items)
	{
		int userId = getCurrentUserId();

		try
		{
			for (Versioned<FoodItem> item : items)
			{
				if (item.getId() == null || item.getId().length() < ObjectService.ID_FULL_SIZE)
				{
					throw new IllegalArgumentException(String.format(Locale.US, "Invalid ID: %s, must be %d characters long", item.getId(),
							ObjectService.ID_FULL_SIZE));
				}

				Map<String, String> set = new HashMap<>();

				set.put(COLUMN_FOODBASE_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
				set.put(COLUMN_FOODBASE_HASH, item.getHash());
				set.put(COLUMN_FOODBASE_VERSION, String.valueOf(item.getVersion()));
				set.put(COLUMN_FOODBASE_DELETED, Utils.formatBooleanInt(item.isDeleted()));
				set.put(COLUMN_FOODBASE_CONTENT, serializer.write(item.getData()));
				set.put(COLUMN_FOODBASE_NAMECACHE, item.getData().getName());

				if (recordExists(userId, item.getId()))
				{
					// presented, update

					Map<String, String> where = new HashMap<>();
					where.put(COLUMN_FOODBASE_GUID, item.getId());
					where.put(COLUMN_FOODBASE_USER, String.valueOf(userId));

					MySQLAccess.update(TABLE_FOODBASE, set, where);
				}
				else
				{
					// not presented, insert

					set.put(COLUMN_FOODBASE_GUID, item.getId());
					set.put(COLUMN_FOODBASE_USER, String.valueOf(userId));

					MySQLAccess.insert(TABLE_FOODBASE, set);
				}

				cachedHashTree.setFoodTree(userId, null);
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
		final String[] select = { COLUMN_FOODBASE_GUID };
		final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_GUID);
		final String[] whereArgs = { String.valueOf(userId), id };
		final String order = null;

		return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<Boolean>()
		{
			@Override
			public Boolean onData(ResultSet set) throws SQLException
			{
				return set.first();
			}
		});
	}

	@Override
	public String exportData()
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?)", COLUMN_FOODBASE_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_FOODBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();
					s.append("[");

					while (resultSet.next())
					{
						String id = resultSet.getString(COLUMN_FOODBASE_GUID);
						String timeStamp = Utils.formatTimeUTC(resultSet.getTimestamp(COLUMN_FOODBASE_TIMESTAMP));
						String hash = resultSet.getString(COLUMN_FOODBASE_HASH);
						int version = resultSet.getInt(COLUMN_FOODBASE_VERSION);
						boolean deleted = (resultSet.getInt(COLUMN_FOODBASE_DELETED) == 1);
						String content = resultSet.getString(COLUMN_FOODBASE_CONTENT);

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

	public String exportPlain()
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?)", COLUMN_FOODBASE_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_FOODBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_FOODBASE, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();

					s.append("VERSION=1\n");
					while (resultSet.next())
					{
						String name = resultSet.getString(COLUMN_FOODBASE_NAMECACHE);
						String id = resultSet.getString(COLUMN_FOODBASE_GUID);
						String timeStamp = Utils.formatTimeUTC(resultSet.getTimestamp(COLUMN_FOODBASE_TIMESTAMP));
						String hash = resultSet.getString(COLUMN_FOODBASE_HASH);
						int version = resultSet.getInt(COLUMN_FOODBASE_VERSION);
						boolean deleted = (resultSet.getInt(COLUMN_FOODBASE_DELETED) == 1);
						String content = resultSet.getString(COLUMN_FOODBASE_CONTENT);

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
}
