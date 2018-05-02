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
package org.bosik.diacomp.web.backend.features.base.food.combo;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;
import org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService;
import org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TimeZone;

import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_CARBS;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_DELETED;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_FATS;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_FROM_TABLE;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_HASH;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_ID;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_LAST_MODIFIED;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_NAME;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_PROTS;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_VALUE;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.COLUMN_FOOD_COMMON_VERSION;
import static org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService.TABLE_FOOD_COMMON;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_CARBS;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_DELETED;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_FATS;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_FROM_TABLE;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_HASH;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_ID;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_LAST_MODIFIED;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_NAME;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_PROTS;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_USER_ID;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_VALUE;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.COLUMN_FOOD_USER_VERSION;
import static org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService.TABLE_FOOD_USER;

@Service
// @Profile("real")
public class FoodComboLocalService
{
	private static final Parser<FoodItem>     parser     = new ParserFoodItem();
	private static final Serializer<FoodItem> serializer = new SerializerAdapter<>(parser);

	@Autowired
	private FoodUserLocalService foodUserLocalService;

	@Autowired
	private FoodCommonLocalService foodCommonLocalService;

	@Autowired
	private CachedFoodComboHashTree cachedHashTree;

	// -------------------------------------------------------------------------------------------------

	private static List<Versioned<FoodItem>> merge(List<Versioned<FoodItem>> foodCommon, List<Versioned<FoodItem>> foodUser)
	{
		Map<String, Versioned<FoodItem>> map = new HashMap<>();
		for (Versioned<FoodItem> f : foodCommon)
		{
			map.put(f.getId(), f);
		}

		for (Versioned<FoodItem> f : foodUser) // user overrides common
		{
			map.put(f.getId(), f);
		}

		return new ArrayList<>(map.values());
	}

	public void add(int userId, Versioned<FoodItem> item) throws PersistenceException
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
			Connection c = MySQLAccess.getConnection();
			PreparedStatement statement = c.prepareStatement(
					"SELECT COUNT(*) FROM (SELECT " + COLUMN_FOOD_COMMON_ID + " FROM " + TABLE_FOOD_COMMON + " WHERE "
							+ COLUMN_FOOD_COMMON_ID + " LIKE ? UNION SELECT " + COLUMN_FOOD_USER_ID + " FROM " + TABLE_FOOD_USER + " WHERE "
							+ COLUMN_FOOD_USER_ID + " LIKE ? AND " + COLUMN_FOOD_USER_USER_ID + " = ?) AS T");

			statement.setString(1, prefix + "%");
			statement.setString(2, prefix + "%");
			statement.setInt(3, userId);

			return MySQLAccess.select(c, statement, new DataCallback<Integer>()
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
		List<Versioned<FoodItem>> foodCommon = foodCommonLocalService.find(includeRemoved);
		List<Versioned<FoodItem>> foodUser = foodUserLocalService.findAll(userId, includeRemoved);
		return merge(foodCommon, foodUser);
	}

	public List<Versioned<FoodItem>> findAny(int userId, String filter)
	{
		List<Versioned<FoodItem>> foodCommon = foodCommonLocalService.findAny(filter);
		List<Versioned<FoodItem>> foodUser = foodUserLocalService.findAny(userId, filter);
		return merge(foodCommon, foodUser);
	}

	public Versioned<FoodItem> findOne(int userId, String exactName)
	{
		Versioned<FoodItem> foodUser = foodUserLocalService.findOne(userId, exactName);

		if (foodUser != null)
		{
			return foodUser;
		}
		else
		{
			return foodCommonLocalService.findOne(exactName);
		}
	}

	public Versioned<FoodItem> findById(int userId, String id)
	{
		Versioned<FoodItem> foodUser = foodUserLocalService.findById(userId, id);

		if (foodUser != null)
		{
			return foodUser;
		}
		else
		{
			return foodCommonLocalService.findById(id);
		}
	}

	public List<Versioned<FoodItem>> findByIdPrefix(int userId, String prefix)
	{
		List<Versioned<FoodItem>> foodCommon = foodCommonLocalService.findByIdPrefix(prefix);
		List<Versioned<FoodItem>> foodUser = foodUserLocalService.findByIdPrefix(userId, prefix);
		return merge(foodCommon, foodUser);
	}

	public List<Versioned<FoodItem>> findChanged(int userId, Date since)
	{
		List<Versioned<FoodItem>> foodCommon = foodCommonLocalService.findChanged(since);
		List<Versioned<FoodItem>> foodUser = foodUserLocalService.findChanged(userId, since);
		return merge(foodCommon, foodUser);
	}

	/**
	 * Returns sorted map (ID, Hash) for all items
	 *
	 * @return
	 */
	@SuppressWarnings("static-method")
	private SortedMap<String, String> getDataHashes(int userId)
	{
		SortedMap<String, String> foodCommon = foodCommonLocalService.getDataHashes();
		SortedMap<String, String> foodUser = foodUserLocalService.getDataHashes(userId);

		foodCommon.putAll(foodUser);
		return foodCommon;
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
		foodUserLocalService.save(userId, items);
	}

	public String exportJson(int userId)
	{
		try
		{
			// SELECT * FROM food_common WHERE ID NOT IN (SELECT _GUID FROM foodbase2 WHERE _UserID = 1);
			// SELECT * FROM foodbase2 WHERE _UserID = 1;

			final StringBuilder s = new StringBuilder();
			s.append("[");

			// fetch common food

			Connection c = MySQLAccess.getConnection();
			PreparedStatement statement = c.prepareStatement(
					"SELECT * FROM " + TABLE_FOOD_COMMON + " WHERE " + COLUMN_FOOD_COMMON_ID + " NOT IN (SELECT " + COLUMN_FOOD_USER_ID
							+ " FROM " + TABLE_FOOD_USER + " WHERE " + COLUMN_FOOD_USER_USER_ID + " = ?)");

			statement.setInt(1, userId);

			MySQLAccess.select(c, statement, new DataCallback<Void>()
			{
				@Override
				public Void onData(ResultSet resultSet) throws SQLException
				{
					while (resultSet.next())
					{
						String id = resultSet.getString(COLUMN_FOOD_COMMON_ID);
						String timeStamp = Utils.formatTimeUTC(resultSet.getTimestamp(COLUMN_FOOD_COMMON_LAST_MODIFIED));
						String hash = resultSet.getString(COLUMN_FOOD_COMMON_HASH);
						int version = resultSet.getInt(COLUMN_FOOD_COMMON_VERSION);
						boolean deleted = resultSet.getBoolean(COLUMN_FOOD_COMMON_DELETED);

						FoodItem food = new FoodItem();

						food.setName(resultSet.getString(COLUMN_FOOD_COMMON_NAME));
						food.setRelProts(resultSet.getDouble(COLUMN_FOOD_COMMON_PROTS));
						food.setRelFats(resultSet.getDouble(COLUMN_FOOD_COMMON_FATS));
						food.setRelCarbs(resultSet.getDouble(COLUMN_FOOD_COMMON_CARBS));
						food.setRelValue(resultSet.getDouble(COLUMN_FOOD_COMMON_VALUE));
						food.setFromTable(resultSet.getBoolean(COLUMN_FOOD_COMMON_FROM_TABLE));

						String content = serializer.write(food);

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

					return null;
				}
			});

			// fetch user food

			final String[] select = null; // all
			final String where = String.format("(%s = ?)", COLUMN_FOOD_USER_USER_ID);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_FOOD_USER_NAME;

			MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<Void>()
			{
				@Override
				public Void onData(ResultSet resultSet) throws SQLException
				{
					while (resultSet.next())
					{
						String id = resultSet.getString(COLUMN_FOOD_USER_ID);
						String timeStamp = Utils.formatTimeUTC(resultSet.getTimestamp(COLUMN_FOOD_USER_LAST_MODIFIED));
						String hash = resultSet.getString(COLUMN_FOOD_USER_HASH);
						int version = resultSet.getInt(COLUMN_FOOD_USER_VERSION);
						boolean deleted = resultSet.getBoolean(COLUMN_FOOD_USER_DELETED);

						FoodItem food = new FoodItem();

						food.setName(resultSet.getString(COLUMN_FOOD_USER_NAME));
						food.setRelProts(resultSet.getDouble(COLUMN_FOOD_USER_PROTS));
						food.setRelFats(resultSet.getDouble(COLUMN_FOOD_USER_FATS));
						food.setRelCarbs(resultSet.getDouble(COLUMN_FOOD_USER_CARBS));
						food.setRelValue(resultSet.getDouble(COLUMN_FOOD_USER_VALUE));
						food.setFromTable(resultSet.getBoolean(COLUMN_FOOD_USER_FROM_TABLE));

						String content = serializer.write(food);

						if (s.length() > 1)
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

					return null;
				}
			});

			s.append("]");

			return s.toString();
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
			final StringBuilder s = new StringBuilder();

			s.append("VERSION=1\n");

			// fetch common food

			Connection c = MySQLAccess.getConnection();
			PreparedStatement statement = c.prepareStatement(
					"SELECT * FROM " + TABLE_FOOD_COMMON + " WHERE " + COLUMN_FOOD_COMMON_ID + " NOT IN (SELECT " + COLUMN_FOOD_USER_ID
							+ " FROM " + TABLE_FOOD_USER + " WHERE " + COLUMN_FOOD_USER_USER_ID + " = ?)");

			statement.setInt(1, userId);

			MySQLAccess.select(c, statement, new DataCallback<Void>()
			{
				@Override
				public Void onData(ResultSet resultSet) throws SQLException
				{
					while (resultSet.next())
					{
						String id = resultSet.getString(COLUMN_FOOD_COMMON_ID);
						String timeStamp = Utils.formatTimeUTC(resultSet.getTimestamp(COLUMN_FOOD_COMMON_LAST_MODIFIED));
						String hash = resultSet.getString(COLUMN_FOOD_COMMON_HASH);
						int version = resultSet.getInt(COLUMN_FOOD_COMMON_VERSION);
						boolean deleted = resultSet.getBoolean(COLUMN_FOOD_COMMON_DELETED);

						FoodItem food = new FoodItem();

						food.setName(resultSet.getString(COLUMN_FOOD_COMMON_NAME));
						food.setRelProts(resultSet.getDouble(COLUMN_FOOD_COMMON_PROTS));
						food.setRelFats(resultSet.getDouble(COLUMN_FOOD_COMMON_FATS));
						food.setRelCarbs(resultSet.getDouble(COLUMN_FOOD_COMMON_CARBS));
						food.setRelValue(resultSet.getDouble(COLUMN_FOOD_COMMON_VALUE));
						food.setFromTable(resultSet.getBoolean(COLUMN_FOOD_COMMON_FROM_TABLE));

						String content = serializer.write(food);

						s.append(Utils.removeTabs(food.getName())).append('\t');
						s.append(id).append('\t');
						s.append(timeStamp).append('\t');
						s.append(hash).append('\t');
						s.append(version).append('\t');
						s.append(deleted).append('\t');
						s.append(content).append('\n');
					}

					return null;
				}
			});

			// fetch user food

			final String[] select = null; // all
			final String where = String.format("(%s = ?)", COLUMN_FOOD_USER_USER_ID);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = COLUMN_FOOD_USER_NAME;

			MySQLAccess.select(TABLE_FOOD_USER, select, where, whereArgs, order, new DataCallback<Void>()
			{
				@Override
				public Void onData(ResultSet resultSet) throws SQLException
				{
					while (resultSet.next())
					{
						String name = resultSet.getString(COLUMN_FOOD_USER_NAME);
						String id = resultSet.getString(COLUMN_FOOD_USER_ID);
						String timeStamp = Utils
								.formatTimeLocal(TimeZone.getDefault(), resultSet.getTimestamp(COLUMN_FOOD_USER_LAST_MODIFIED));
						String hash = resultSet.getString(COLUMN_FOOD_USER_HASH);
						int version = resultSet.getInt(COLUMN_FOOD_USER_VERSION);
						boolean deleted = resultSet.getBoolean(COLUMN_FOOD_USER_DELETED);

						FoodItem food = new FoodItem();

						food.setName(resultSet.getString(COLUMN_FOOD_USER_NAME));
						food.setRelProts(resultSet.getDouble(COLUMN_FOOD_USER_PROTS));
						food.setRelFats(resultSet.getDouble(COLUMN_FOOD_USER_FATS));
						food.setRelCarbs(resultSet.getDouble(COLUMN_FOOD_USER_CARBS));
						food.setRelValue(resultSet.getDouble(COLUMN_FOOD_USER_VALUE));
						food.setFromTable(resultSet.getBoolean(COLUMN_FOOD_USER_FROM_TABLE));

						String content = serializer.write(food);

						s.append(Utils.removeTabs(name)).append('\t');
						s.append(id).append('\t');
						s.append(timeStamp).append('\t');
						s.append(hash).append('\t');
						s.append(version).append('\t');
						s.append(deleted).append('\t');
						s.append(content).append('\n');
					}

					return null;
				}
			});

			return s.toString();
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}
}
