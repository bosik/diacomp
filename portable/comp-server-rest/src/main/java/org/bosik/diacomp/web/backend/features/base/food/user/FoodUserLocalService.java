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
package org.bosik.diacomp.web.backend.features.base.food.user;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
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
public class FoodUserLocalService
{
	// Foodbase table (legacy)
	private static final String TABLE_FOODBASE            = "foodbase2";
	private static final String COLUMN_FOODBASE_GUID      = "_GUID";
	private static final String COLUMN_FOODBASE_USER      = "_UserID";
	private static final String COLUMN_FOODBASE_TIMESTAMP = "_TimeStamp";
	private static final String COLUMN_FOODBASE_HASH      = "_Hash";
	private static final String COLUMN_FOODBASE_VERSION   = "_Version";
	private static final String COLUMN_FOODBASE_DELETED   = "_Deleted";
	private static final String COLUMN_FOODBASE_CONTENT   = "_Content";

	// Foodbase table
	public static final String TABLE_FOOD_USER                = "food_user";
	public static final String COLUMN_FOOD_USER_ID            = "ID";
	public static final String COLUMN_FOOD_USER_USER_ID       = "UserID";
	public static final String COLUMN_FOOD_USER_NAME          = "Name";
	public static final String COLUMN_FOOD_USER_PROTS         = "Prots";
	public static final String COLUMN_FOOD_USER_FATS          = "Fats";
	public static final String COLUMN_FOOD_USER_CARBS         = "Carbs";
	public static final String COLUMN_FOOD_USER_VALUE         = "Value";
	public static final String COLUMN_FOOD_USER_FROM_TABLE    = "FromTable"; // *
	public static final String COLUMN_FOOD_USER_DELETED       = "Deleted";
	public static final String COLUMN_FOOD_USER_LAST_MODIFIED = "LastModified";
	public static final String COLUMN_FOOD_USER_HASH          = "Hash"; // *
	public static final String COLUMN_FOOD_USER_VERSION       = "Version"; // *

	private static final int MAX_READ_ITEMS = 500;

	private static final Parser<FoodItem>     parser     = new ParserFoodItem();
	private static final Serializer<FoodItem> serializer = new SerializerAdapter<>(parser);

	@Autowired
	private CachedFoodUserHashTree cachedHashTree;

	private static Versioned<FoodItem> parseItem(ResultSet resultSet) throws SQLException
	{
		final FoodItem food = new FoodItem();
		food.setName(resultSet.getString(COLUMN_FOOD_USER_NAME));
		food.setRelProts(resultSet.getDouble(COLUMN_FOOD_USER_PROTS));
		food.setRelFats(resultSet.getDouble(COLUMN_FOOD_USER_FATS));
		food.setRelCarbs(resultSet.getDouble(COLUMN_FOOD_USER_CARBS));
		food.setRelValue(resultSet.getDouble(COLUMN_FOOD_USER_VALUE));
		food.setFromTable(resultSet.getBoolean(COLUMN_FOOD_USER_FROM_TABLE));

		Versioned<FoodItem> item = new Versioned<>();
		item.setId(resultSet.getString(COLUMN_FOOD_USER_ID));
		item.setTimeStamp(Utils.parseTimeUTC(resultSet.getString(COLUMN_FOOD_USER_LAST_MODIFIED)));
		item.setHash(resultSet.getString(COLUMN_FOOD_USER_HASH));
		item.setVersion(resultSet.getInt(COLUMN_FOOD_USER_VERSION));
		item.setDeleted(resultSet.getBoolean(COLUMN_FOOD_USER_DELETED));
		item.setData(food);

		return item;
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
			result.add(parseItem(resultSet));
		}

		return result;
	}

	private static List<Versioned<FoodItem>> parseItems(ResultSet resultSet) throws SQLException
	{
		return parseItems(resultSet, 0);
	}

	public void add(int userId, Versioned<FoodItem> item) throws DuplicateException, PersistenceException
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
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_FOOD_USER_USER_ID, COLUMN_FOOD_USER_ID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = null;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<Integer>()
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
		Versioned<FoodItem> item = findById(userId, id);

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

	public List<Versioned<FoodItem>> findAll(int userId, boolean includeRemoved)
	{
		try
		{
			final String[] select = null; // all
			String where;
			String[] whereArgs;

			if (includeRemoved)
			{
				where = String.format("(%s = ?)", COLUMN_FOOD_USER_USER_ID);
				whereArgs = new String[] { String.valueOf(userId) };
			}
			else
			{
				where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOOD_USER_USER_ID, COLUMN_FOOD_USER_DELETED);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatBooleanInt(false) };
			}

			final String order = null;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<List<Versioned<FoodItem>>>()
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

	public List<Versioned<FoodItem>> findAny(int userId, String filter)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?) AND (%s LIKE ?)", COLUMN_FOOD_USER_USER_ID, COLUMN_FOOD_USER_DELETED,
					COLUMN_FOOD_USER_NAME);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), "%" + filter + "%" };
			final String order = COLUMN_FOOD_USER_NAME;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<List<Versioned<FoodItem>>>()
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

	public Versioned<FoodItem> findById(int userId, String id)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOOD_USER_USER_ID, COLUMN_FOOD_USER_ID);
			final String[] whereArgs = { String.valueOf(userId), id };
			final String order = null;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<Versioned<FoodItem>>()
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

	public List<Versioned<FoodItem>> findByIdPrefix(int userId, String prefix)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_FOOD_USER_USER_ID, COLUMN_FOOD_USER_ID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = COLUMN_FOOD_USER_NAME;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<List<Versioned<FoodItem>>>()
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

	public List<Versioned<FoodItem>> findChanged(int userId, Date since)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s >= ?)", COLUMN_FOOD_USER_USER_ID, COLUMN_FOOD_USER_LAST_MODIFIED);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatTimeUTC(since) };
			final String order = COLUMN_FOOD_USER_NAME;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<List<Versioned<FoodItem>>>()
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

	public Versioned<FoodItem> findOne(int userId, String exactName)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?) AND (%s = ?)", COLUMN_FOOD_USER_USER_ID, COLUMN_FOOD_USER_DELETED,
					COLUMN_FOOD_USER_NAME);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), exactName };
			final String order = null;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<Versioned<FoodItem>>()
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
	public SortedMap<String, String> getDataHashes(int userId)
	{
		try
		{
			final String[] select = { COLUMN_FOOD_USER_ID, COLUMN_FOOD_USER_HASH };
			final String where = String.format("(%s = ?)", COLUMN_FOOD_USER_USER_ID);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<SortedMap<String, String>>()
			{
				@Override
				public SortedMap<String, String> onData(ResultSet set) throws SQLException
				{
					SortedMap<String, String> result = new TreeMap<String, String>();

					while (set.next())
					{
						String id = set.getString(COLUMN_FOOD_USER_ID);
						String hash = set.getString(COLUMN_FOOD_USER_HASH);
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

	public void save(int userId, List<Versioned<FoodItem>> items)
	{
		try
		{
			for (Versioned<FoodItem> item : items)
			{
				if (item.getId() == null || item.getId().length() < ObjectService.ID_FULL_SIZE)
				{
					throw new IllegalArgumentException(String.format(Locale.US, "Invalid ID: %s, must be %d characters long", item.getId(),
							ObjectService.ID_FULL_SIZE));
				}

				Map<String, Object> set = new HashMap<>();

				set.put(COLUMN_FOOD_USER_NAME, item.getData().getName());
				set.put(COLUMN_FOOD_USER_PROTS, item.getData().getRelProts());
				set.put(COLUMN_FOOD_USER_FATS, item.getData().getRelFats());
				set.put(COLUMN_FOOD_USER_CARBS, item.getData().getRelCarbs());
				set.put(COLUMN_FOOD_USER_VALUE, item.getData().getRelValue());
				set.put(COLUMN_FOOD_USER_FROM_TABLE, item.getData().getFromTable());

				set.put(COLUMN_FOOD_USER_DELETED, item.isDeleted());
				set.put(COLUMN_FOOD_USER_LAST_MODIFIED, Utils.formatTimeUTC(item.getTimeStamp()));
				set.put(COLUMN_FOOD_USER_HASH, item.getHash());
				set.put(COLUMN_FOOD_USER_VERSION, item.getVersion());

				if (recordExists(userId, item.getId()))
				{
					// presented, update

					Map<String, String> where = new HashMap<>();
					where.put(COLUMN_FOOD_USER_ID, item.getId());
					where.put(COLUMN_FOOD_USER_USER_ID, String.valueOf(userId));

					MySQLAccess.update(TABLE_FOOD_USER, set, where);
				}
				else
				{
					// not presented, insert

					set.put(COLUMN_FOOD_USER_ID, item.getId());
					set.put(COLUMN_FOOD_USER_USER_ID, String.valueOf(userId));

					MySQLAccess.insert(TABLE_FOOD_USER, set);
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
		final String[] select = { COLUMN_FOOD_USER_ID };
		final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOOD_USER_USER_ID, COLUMN_FOOD_USER_ID);
		final String[] whereArgs = { String.valueOf(userId), id };
		final String order = null;

		return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<Boolean>()
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
			final String where = String.format("(%s = ?)", COLUMN_FOOD_USER_USER_ID);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_FOOD_USER_NAME;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();
					s.append("[");

					while (resultSet.next())
					{
						Versioned<FoodItem> item = parseItem(resultSet);

						if (!resultSet.isFirst())
						{
							s.append(",");
						}

						s.append("{");
						s.append("\"id\":\"").append(item.getId()).append("\",");
						s.append("\"stamp\":\"").append(Utils.formatTimeUTC(item.getTimeStamp())).append("\",");
						s.append("\"hash\":\"").append(item.getHash()).append("\",");
						s.append("\"version\":").append(item.getVersion()).append(",");
						s.append("\"deleted\":").append(item.isDeleted()).append(",");
						s.append("\"data\":").append(serializer.write(item.getData()));
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
			final String where = String.format("(%s = ?)", COLUMN_FOOD_USER_USER_ID);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_FOOD_USER_NAME;

			return MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();

					s.append("VERSION=1\n");
					while (resultSet.next())
					{
						Versioned<FoodItem> item = parseItem(resultSet);

						s.append(Utils.removeTabs(item.getData().getName())).append('\t');
						s.append(item.getId()).append('\t');
						s.append(Utils.formatTimeUTC(item.getTimeStamp())).append('\t');
						s.append(item.getHash()).append('\t');
						s.append(item.getVersion()).append('\t');
						s.append(item.isDeleted()).append('\t');
						s.append(serializer.write(item.getData())).append('\n');
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
