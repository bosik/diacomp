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

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodSetInfo;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;

public class FoodSetService
{
	// Foodbase table
	private static final String	TABLE_FOODSET				= "foodset";
	private static final String	COLUMN_FOODSET_ID			= "_ID";
	private static final String	COLUMN_FOODSET_DESCRIPTION	= "_Description";
	private static final String	COLUMN_FOODSET_SIZE			= "_Size";
	private static final String	COLUMN_FOODSET_DATA			= "_Data";

	@SuppressWarnings("static-method")
	public String getFoodSet(String id)
	{
		try
		{
			final String[] select = { COLUMN_FOODSET_DATA };
			String where = String.format("(%s = ?)", COLUMN_FOODSET_ID);
			String[] whereArgs = new String[] { id };
			final String order = null;

			return MySQLAccess.select(TABLE_FOODSET, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						String content = set.getString(COLUMN_FOODSET_DATA);
						return content;
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

	@SuppressWarnings("static-method")
	public List<FoodSetInfo> getFoodSetInfo()
	{
		try
		{
			final String[] select = { COLUMN_FOODSET_ID, COLUMN_FOODSET_DESCRIPTION, COLUMN_FOODSET_SIZE };
			// TODO: support empty WHERE clauses
			String where = "1=1";
			String[] whereArgs = {};
			final String order = null;

			return MySQLAccess.select(TABLE_FOODSET, select, where, whereArgs, order,
					new DataCallback<List<FoodSetInfo>>()
					{
						@Override
						public List<FoodSetInfo> onData(ResultSet set) throws SQLException
						{
							List<FoodSetInfo> list = new ArrayList<FoodSetInfo>();

							while (set.next())
							{
								String id = set.getString(COLUMN_FOODSET_ID);
								String description = set.getString(COLUMN_FOODSET_DESCRIPTION);
								int size = set.getInt(COLUMN_FOODSET_SIZE);

								FoodSetInfo info = new FoodSetInfo();
								info.setId(id);
								info.setDescription(description);
								info.setSize(size);

								list.add(info);
							}

							return list;
						}
					});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}
}
