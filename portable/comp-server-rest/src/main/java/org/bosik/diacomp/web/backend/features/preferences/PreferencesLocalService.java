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
package org.bosik.diacomp.web.backend.features.preferences;

import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesService;
import org.bosik.diacomp.core.services.preferences.PreferencesServiceContract;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;
import org.json.JSONObject;
import org.springframework.stereotype.Service;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

@Service
// @Profile({ "real", "fake" })
public class PreferencesLocalService
{
	private static final String TABLE_PREFERENCES          = "preferences";
	private static final String COLUMN_PREFERENCES_USER    = "_UserID";
	private static final String COLUMN_PREFERENCES_KEY     = "_Key";
	private static final String COLUMN_PREFERENCES_VALUE   = "_Value";
	private static final String COLUMN_PREFERENCES_VERSION = "_Version";

	public String getHash(int userId)
	{
		return PreferencesServiceContract.getHash(getAll(userId));
	}

	public List<PreferenceEntry<String>> getAll(int userId)
	{
		try
		{
			final String[] select = { COLUMN_PREFERENCES_KEY, COLUMN_PREFERENCES_VALUE, COLUMN_PREFERENCES_VERSION };
			final String where = String.format("(%s = ?)", COLUMN_PREFERENCES_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			return MySQLAccess.select(TABLE_PREFERENCES, select, where, whereArgs, order, new DataCallback<List<PreferenceEntry<String>>>()
			{
				@Override
				public List<PreferenceEntry<String>> onData(ResultSet set) throws SQLException
				{
					List<PreferenceEntry<String>> result = new LinkedList<>();

					while (set.next())
					{
						String key = set.getString(COLUMN_PREFERENCES_KEY);
						String value = set.getString(COLUMN_PREFERENCES_VALUE);
						int version = set.getInt(COLUMN_PREFERENCES_VERSION);

						try
						{
							PreferenceEntry<String> item = new PreferenceEntry<>();
							item.setId(PreferenceID.parse(key)); // TODO
							item.setValue(value);
							item.setVersion(version);
							result.add(item);
						}
						catch (IllegalArgumentException e)
						{
									/**/
							System.out.println("Failed to parse preference type: " + key);
									/**/
							e.printStackTrace();
						}
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

	public void update(int userId, List<PreferenceEntry<String>> entries)
	{
		for (PreferenceEntry<String> entry : entries)
		{
			setString(userId, entry);
		}
	}

	public PreferenceEntry<String> getString(int userId, final PreferenceID id)
	{
		try
		{
			final String[] select = { COLUMN_PREFERENCES_VALUE, COLUMN_PREFERENCES_VERSION };
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_PREFERENCES_USER, COLUMN_PREFERENCES_KEY);
			final String[] whereArgs = { String.valueOf(userId), id.getCode() };
			final String order = null;

			return MySQLAccess.select(TABLE_PREFERENCES, select, where, whereArgs, order, new DataCallback<PreferenceEntry<String>>()
			{
				@Override
				public PreferenceEntry<String> onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						PreferenceEntry<String> item = new PreferenceEntry<>();
						item.setId(id);
						item.setValue(set.getString(COLUMN_PREFERENCES_VALUE));
						item.setVersion(set.getInt(COLUMN_PREFERENCES_VERSION));

						return item;
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

	public void setString(int userId, PreferenceEntry<String> entry)
	{
		try
		{
			final String key = entry.getId().getCode();
			final String value = entry.getValue();
			final String version = String.valueOf(entry.getVersion());

			if (recordExists(userId, key))
			{
				// presented, update

				SortedMap<String, String> set = new TreeMap<>();
				set.put(COLUMN_PREFERENCES_VALUE, value);
				set.put(COLUMN_PREFERENCES_VERSION, version);

				SortedMap<String, String> where = new TreeMap<>();
				where.put(COLUMN_PREFERENCES_KEY, key);
				where.put(COLUMN_PREFERENCES_USER, String.valueOf(userId));

				MySQLAccess.update(TABLE_PREFERENCES, set, where);
			}
			else
			{
				// not presented, insert

				LinkedHashMap<String, String> set = new LinkedHashMap<>();
				set.put(COLUMN_PREFERENCES_USER, String.valueOf(userId));
				set.put(COLUMN_PREFERENCES_KEY, key);
				set.put(COLUMN_PREFERENCES_VALUE, value);
				set.put(COLUMN_PREFERENCES_VERSION, version);

				MySQLAccess.insert(TABLE_PREFERENCES, set);
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@SuppressWarnings("static-method")
	private boolean recordExists(int userId, String key) throws SQLException
	{
		final String[] select = { "COUNT(*)" };
		final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_PREFERENCES_USER, COLUMN_PREFERENCES_KEY);
		final String[] whereArgs = { String.valueOf(userId), key };
		final String order = null;

		return MySQLAccess.select(TABLE_PREFERENCES, select, where, whereArgs, order, new DataCallback<Boolean>()
		{
			@Override
			public Boolean onData(ResultSet set) throws SQLException
			{
				if (set.next())
				{
					return set.getInt(1) > 0;
				}
				else
				{
					throw new IllegalStateException("Failed to request SQL database");
				}
			}
		});
	}

	public String exportData(int userId)
	{
		try
		{
			final String[] select = { COLUMN_PREFERENCES_KEY, COLUMN_PREFERENCES_VALUE, COLUMN_PREFERENCES_VERSION };
			final String where = String.format("(%s = ?)", COLUMN_PREFERENCES_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			return MySQLAccess.select(TABLE_PREFERENCES, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();
					s.append("[");

					while (resultSet.next())
					{
						final String key = resultSet.getString(COLUMN_PREFERENCES_KEY);
						final String value = resultSet.getString(COLUMN_PREFERENCES_VALUE);
						final int version = resultSet.getInt(COLUMN_PREFERENCES_VERSION);

						if (!resultSet.isFirst())
						{
							s.append(",");
						}

						s.append(new JSONObject()
						{
							{
								put("key", key);
								put("value", value);
								put("version", version);
							}
						}.toString());
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
			final String[] select = { COLUMN_PREFERENCES_KEY, COLUMN_PREFERENCES_VALUE, COLUMN_PREFERENCES_VERSION };
			final String where = String.format("(%s = ?)", COLUMN_PREFERENCES_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			return MySQLAccess.select(TABLE_PREFERENCES, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet resultSet) throws SQLException
				{
					StringBuilder s = new StringBuilder();
					s.append("VERSION=1\n");

					while (resultSet.next())
					{
						String key = resultSet.getString(COLUMN_PREFERENCES_KEY);
						int version = resultSet.getInt(COLUMN_PREFERENCES_VERSION);
						String value = resultSet.getString(COLUMN_PREFERENCES_VALUE);

						s.append(key).append('\t');
						s.append(version).append('\t');
						s.append(value).append('\n');
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
