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
package org.bosik.diacomp.web.backend.features.base.food.common;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.merklesync.Versioned;
import org.springframework.stereotype.Service;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

@Service
public class FoodCommonLocalService
{
	// DB
	public static final String TABLE_FOOD_COMMON                = "food_common";
	public static final String COLUMN_FOOD_COMMON_ID            = "ID";
	public static final String COLUMN_FOOD_COMMON_NAME          = "Name";
	public static final String COLUMN_FOOD_COMMON_PROTS         = "Prots";
	public static final String COLUMN_FOOD_COMMON_FATS          = "Fats";
	public static final String COLUMN_FOOD_COMMON_CARBS         = "Carbs";
	public static final String COLUMN_FOOD_COMMON_VALUE         = "Value";
	public static final String COLUMN_FOOD_COMMON_FROM_TABLE    = "FromTable"; // *
	public static final String COLUMN_FOOD_COMMON_DELETED       = "Deleted";
	public static final String COLUMN_FOOD_COMMON_LAST_MODIFIED = "LastModified";
	public static final String COLUMN_FOOD_COMMON_HASH          = "Hash"; // *
	public static final String COLUMN_FOOD_COMMON_VERSION       = "Version"; // *
	public static final String COLUMN_FOOD_COMMON_TAG           = "Tag"; // *

	private static List<Versioned<FoodItem>> parseItems(ResultSet set) throws SQLException
	{
		List<Versioned<FoodItem>> list = new ArrayList<>();

		while (set.next())
		{
			FoodItem food = new FoodItem();

			food.setName(set.getString(COLUMN_FOOD_COMMON_NAME));
			food.setRelProts(set.getDouble(COLUMN_FOOD_COMMON_PROTS));
			food.setRelFats(set.getDouble(COLUMN_FOOD_COMMON_FATS));
			food.setRelCarbs(set.getDouble(COLUMN_FOOD_COMMON_CARBS));
			food.setRelValue(set.getDouble(COLUMN_FOOD_COMMON_VALUE));
			food.setFromTable(set.getBoolean(COLUMN_FOOD_COMMON_FROM_TABLE)); // TODO: test it

			Versioned<FoodItem> item = new Versioned<>();

			item.setId(set.getString(COLUMN_FOOD_COMMON_ID));
			item.setData(food);
			item.setDeleted(set.getInt(COLUMN_FOOD_COMMON_DELETED) == 1);
			item.setTimeStamp(Utils.parseTimeUTC(set.getString(COLUMN_FOOD_COMMON_LAST_MODIFIED)));
			item.setHash(set.getString(COLUMN_FOOD_COMMON_HASH));
			item.setVersion(set.getInt(COLUMN_FOOD_COMMON_VERSION));
			// item.setTag(set.getString(COLUMN_FOOD_COMMON_TAG));

			list.add(item);
		}

		return list;
	}

	public void upload(final String tag, List<Versioned<FoodItem>> items)
	{
		try
		{
			for (final Versioned<FoodItem> item : items)
			{
				Map<String, String> fieldsUpdate = new HashMap<String, String>()
				{
					{
						put(COLUMN_FOOD_COMMON_NAME, item.getData().getName());
						put(COLUMN_FOOD_COMMON_PROTS, String.format(Locale.US, "%.2f", item.getData().getRelProts()));
						put(COLUMN_FOOD_COMMON_FATS, String.format(Locale.US, "%.2f", item.getData().getRelFats()));
						put(COLUMN_FOOD_COMMON_CARBS, String.format(Locale.US, "%.2f", item.getData().getRelCarbs()));
						put(COLUMN_FOOD_COMMON_VALUE, String.format(Locale.US, "%.2f", item.getData().getRelValue()));
						put(COLUMN_FOOD_COMMON_DELETED, Utils.formatBooleanInt(item.isDeleted()));
						put(COLUMN_FOOD_COMMON_LAST_MODIFIED, Utils.formatTimeUTC(item.getTimeStamp()));
						put(COLUMN_FOOD_COMMON_TAG, tag);
					}
				};

				Map<String, String> fieldsWhere = new HashMap<String, String>()
				{
					{

						put(COLUMN_FOOD_COMMON_ID, item.getId());
					}
				};

				MySQLAccess.update(TABLE_FOOD_COMMON, fieldsUpdate, fieldsWhere);
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	public List<Versioned<FoodItem>> find()
	{
		try
		{
			final String[] select = null; // all
			// TODO: support empty WHERE clauses
			String where = "1=1";
			String[] whereArgs = {};
			final String order = null;

			return MySQLAccess
					.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<List<Versioned<FoodItem>>>()
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

	public List<Versioned<FoodItem>> find(boolean includeRemoved)
	{
		if (includeRemoved)
		{
			return find();
		}

		try
		{
			final String[] select = null; // all
			String where = String.format("%s = ?", COLUMN_FOOD_COMMON_DELETED);
			String[] whereArgs = { Utils.formatBooleanInt(false) };
			final String order = null;

			return MySQLAccess
					.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<List<Versioned<FoodItem>>>()
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

	/**
	 * Returns items having LastModified more than specified argument
	 *
	 * @param lastModified
	 * @return
	 */
	public List<Versioned<FoodItem>> findChanged(Date lastModified)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s > ?)", COLUMN_FOOD_COMMON_LAST_MODIFIED);
			final String[] whereArgs = { Utils.formatTimeUTC(lastModified) };
			final String order = null;

			return MySQLAccess
					.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<List<Versioned<FoodItem>>>()
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

	public List<Versioned<FoodItem>> findAny(String filter)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_FOOD_COMMON_DELETED, COLUMN_FOOD_COMMON_NAME);
			final String[] whereArgs = { Utils.formatBooleanInt(false), "%" + filter + "%" };
			final String order = COLUMN_FOOD_COMMON_NAME;

			return MySQLAccess
					.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<List<Versioned<FoodItem>>>()
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

	public Versioned<FoodItem> findById(String id)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?)", COLUMN_FOOD_COMMON_ID);
			final String[] whereArgs = { id };
			final String order = null;

			return MySQLAccess
					.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<Versioned<FoodItem>>()
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

	public List<Versioned<FoodItem>> findByIdPrefix(String prefix)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s LIKE ?)", COLUMN_FOOD_COMMON_ID);
			final String[] whereArgs = { prefix + "%" };
			final String order = COLUMN_FOOD_COMMON_NAME;

			return MySQLAccess
					.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<List<Versioned<FoodItem>>>()
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

	public Versioned<FoodItem> findOne(String exactName)
	{
		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOOD_COMMON_DELETED, COLUMN_FOOD_COMMON_NAME);
			final String[] whereArgs = { Utils.formatBooleanInt(false), exactName };
			final String order = null;

			return MySQLAccess
					.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<Versioned<FoodItem>>()
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

	public SortedMap<String, String> getDataHashes()
	{
		try
		{
			final String[] select = { COLUMN_FOOD_COMMON_ID, COLUMN_FOOD_COMMON_HASH };
			final String where = "1 = 1"; // TODO: support empty WHERE clauses
			final String[] whereArgs = {};
			final String order = null;

			return MySQLAccess
					.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<SortedMap<String, String>>()
					{
						@Override
						public SortedMap<String, String> onData(ResultSet set) throws SQLException
						{
							SortedMap<String, String> result = new TreeMap<String, String>();

							while (set.next())
							{
								String id = set.getString(COLUMN_FOOD_COMMON_ID);
								String hash = set.getString(COLUMN_FOOD_COMMON_HASH);
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
}
