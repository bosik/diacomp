package org.bosik.diacomp.web.backend.features.preferences;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.services.preferences.Preference;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("real")
public class PreferencesLocalService extends PreferencesTypedService
{
	private static final String	TABLE_PREFERENCES			= "preferences";
	private static final String	COLUMN_PREFERENCES_USER		= "_UserID";
	private static final String	COLUMN_PREFERENCES_KEY		= "_Key";
	private static final String	COLUMN_PREFERENCES_VALUE	= "_Value";
	private static final String	COLUMN_PREFERENCES_VERSION	= "_Version";

	@Autowired
	private UserInfoService		userInfoService;

	@Override
	public List<PreferenceEntry<String>> getAll()
	{
		int userId = userInfoService.getCurrentUserId();

		try
		{
			final String[] select = { COLUMN_PREFERENCES_KEY, COLUMN_PREFERENCES_VALUE, COLUMN_PREFERENCES_VERSION };
			final String where = String.format("(%s = ?)", COLUMN_PREFERENCES_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			return MySQLAccess.select(TABLE_PREFERENCES, select, where, whereArgs, order,
					new DataCallback<List<PreferenceEntry<String>>>()
					{
						@Override
						public List<PreferenceEntry<String>> onData(ResultSet set) throws SQLException
						{
							List<PreferenceEntry<String>> result = new LinkedList<PreferenceEntry<String>>();

							while (set.next())
							{
								String key = set.getString(COLUMN_PREFERENCES_KEY);
								String value = set.getString(COLUMN_PREFERENCES_VALUE);
								int version = set.getInt(COLUMN_PREFERENCES_VERSION);

								try
								{
									PreferenceEntry<String> item = new PreferenceEntry<String>();
									item.setType(Preference.parse(key)); // TODO
									item.setValue(value);
									item.setVersion(version);
									result.add(item);
								}
								catch (IllegalArgumentException e)
								{
									/**/System.out.println("Failed to parse preference type: " + key);
									/**/e.printStackTrace();
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

	@Override
	public void update(List<PreferenceEntry<String>> entries)
	{
		for (PreferenceEntry<String> entry : entries)
		{
			setString(entry);
		}
	}

	@Override
	public PreferenceEntry<String> getString(final Preference preference)
	{
		int userId = userInfoService.getCurrentUserId();

		try
		{
			final String[] select = { COLUMN_PREFERENCES_VALUE, COLUMN_PREFERENCES_VERSION };
			final String where = String
					.format("(%s = ?) AND (%s = ?)", COLUMN_PREFERENCES_USER, COLUMN_PREFERENCES_KEY);
			final String[] whereArgs = { String.valueOf(userId), preference.getKey() };
			final String order = null;

			return MySQLAccess.select(TABLE_PREFERENCES, select, where, whereArgs, order,
					new DataCallback<PreferenceEntry<String>>()
					{
						@Override
						public PreferenceEntry<String> onData(ResultSet set) throws SQLException
						{
							if (set.next())
							{
								PreferenceEntry<String> item = new PreferenceEntry<String>();
								item.setType(preference);
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

	@Override
	public void setString(PreferenceEntry<String> entry)
	{
		int userId = userInfoService.getCurrentUserId();

		try
		{
			final String key = entry.getType().getKey();
			final String value = entry.getValue();
			final String version = String.valueOf(entry.getVersion());

			if (recordExists(userId, key))
			{
				// presented, update

				SortedMap<String, String> set = new TreeMap<String, String>();
				set.put(COLUMN_PREFERENCES_VALUE, value);
				set.put(COLUMN_PREFERENCES_VERSION, version);

				SortedMap<String, String> where = new TreeMap<String, String>();
				where.put(COLUMN_PREFERENCES_KEY, key);
				where.put(COLUMN_PREFERENCES_USER, String.valueOf(userId));

				MySQLAccess.update(TABLE_PREFERENCES, set, where);
			}
			else
			{
				// not presented, insert

				LinkedHashMap<String, String> set = new LinkedHashMap<String, String>();
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
}
