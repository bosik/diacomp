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

import org.bosik.diacomp.core.entities.business.foodbase.FoodCommon;
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

@Service
public class FoodCommonLocalService
{
	// DB
	private static final String TABLE_FOOD_COMMON                = "food_common";
	private static final String COLUMN_FOOD_COMMON_ID            = "ID";
	private static final String COLUMN_FOOD_COMMON_NAME          = "Name";
	private static final String COLUMN_FOOD_COMMON_PROTS         = "Prots";
	private static final String COLUMN_FOOD_COMMON_FATS          = "Fats";
	private static final String COLUMN_FOOD_COMMON_CARBS         = "Carbs";
	private static final String COLUMN_FOOD_COMMON_VALUE         = "Value";
	private static final String COLUMN_FOOD_COMMON_DELETED       = "Deleted";
	private static final String COLUMN_FOOD_COMMON_LAST_MODIFIED = "LastModified";
	private static final String COLUMN_FOOD_COMMON_TAG           = "Tag";

	private static List<FoodCommon> parseItems(ResultSet set) throws SQLException
	{
		List<FoodCommon> list = new ArrayList<>();

		while (set.next())
		{
			FoodCommon food = new FoodCommon();

			food.setId(set.getString(COLUMN_FOOD_COMMON_ID));
			food.setName(set.getString(COLUMN_FOOD_COMMON_NAME));
			food.setRelProts(set.getDouble(COLUMN_FOOD_COMMON_PROTS));
			food.setRelFats(set.getDouble(COLUMN_FOOD_COMMON_FATS));
			food.setRelCarbs(set.getDouble(COLUMN_FOOD_COMMON_CARBS));
			food.setRelValue(set.getDouble(COLUMN_FOOD_COMMON_VALUE));

			food.setDeleted(set.getInt(COLUMN_FOOD_COMMON_DELETED) == 1);
			food.setLastModified(Utils.parseTimeUTC(set.getString(COLUMN_FOOD_COMMON_LAST_MODIFIED)));
			food.setTag(set.getString(COLUMN_FOOD_COMMON_TAG));

			list.add(food);
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

	public List<FoodCommon> find()
	{
		try
		{
			final String[] select = { COLUMN_FOOD_COMMON_ID, COLUMN_FOOD_COMMON_NAME, COLUMN_FOOD_COMMON_PROTS, COLUMN_FOOD_COMMON_FATS,
					COLUMN_FOOD_COMMON_CARBS, COLUMN_FOOD_COMMON_VALUE, COLUMN_FOOD_COMMON_DELETED, COLUMN_FOOD_COMMON_LAST_MODIFIED,
					COLUMN_FOOD_COMMON_TAG };
			// TODO: support empty WHERE clauses
			String where = "1=1";
			String[] whereArgs = {};
			final String order = null;

			return MySQLAccess.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<List<FoodCommon>>()
			{
				@Override
				public List<FoodCommon> onData(ResultSet set) throws SQLException
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
	public List<FoodCommon> find(Date lastModified)
	{
		try
		{
			final String[] select = { COLUMN_FOOD_COMMON_ID, COLUMN_FOOD_COMMON_NAME, COLUMN_FOOD_COMMON_PROTS, COLUMN_FOOD_COMMON_FATS,
					COLUMN_FOOD_COMMON_CARBS, COLUMN_FOOD_COMMON_VALUE, COLUMN_FOOD_COMMON_DELETED, COLUMN_FOOD_COMMON_LAST_MODIFIED,
					COLUMN_FOOD_COMMON_TAG };
			final String where = String.format("(%s > ?)", COLUMN_FOOD_COMMON_LAST_MODIFIED);
			final String[] whereArgs = { Utils.formatTimeUTC(lastModified) };
			final String order = null;

			return MySQLAccess.select(TABLE_FOOD_COMMON, select, where, whereArgs, order, new MySQLAccess.DataCallback<List<FoodCommon>>()
			{
				@Override
				public List<FoodCommon> onData(ResultSet set) throws SQLException
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
}
