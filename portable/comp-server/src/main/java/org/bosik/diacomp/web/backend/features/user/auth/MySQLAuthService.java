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
package org.bosik.diacomp.web.backend.features.user.auth;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotActivatedException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;
import org.springframework.stereotype.Service;

@Service
public class MySQLAuthService implements AuthService
{
	private static final String	TABLE_USER					= "user";
	private static final String	COLUMN_USER_ID				= "ID";
	private static final String	COLUMN_USER_NAME			= "Login";
	private static final String	COLUMN_USER_HASHPASS		= "HashPass";
	private static final String	COLUMN_USER_ACTIVATION_KEY	= "ActivationKey";
	private static final String	COLUMN_USER_DATE_REG		= "DateReg";
	private static final String	COLUMN_USER_DATE_LOGIN		= "DateLogin";

	public static final int		PASSWORD_MIN_SIZE			= 6;

	@Override
	public String register(String userName, String password)
	{
		if (userName == null || userName.trim().isEmpty())
		{
			throw new IllegalArgumentException("User name is empty");
		}

		if (password == null || password.trim().length() < PASSWORD_MIN_SIZE)
		{
			throw new IllegalArgumentException("Password is too short: " + password);
		}

		if (getIdByName(userName) != null)
		{
			// TODO: use another DuplicateException
			throw new DuplicateException(userName);
		}

		String hashPass = HashUtils.createHash(password);
		String activationCode = org.bosik.merklesync.HashUtils.generateGuid() + org.bosik.merklesync.HashUtils.generateGuid();
		String dateReg = Utils.formatTimeUTC(new Date());

		try
		{
			SortedMap<String, String> set = new TreeMap<String, String>();
			set.put(COLUMN_USER_NAME, userName);
			set.put(COLUMN_USER_HASHPASS, hashPass);
			set.put(COLUMN_USER_ACTIVATION_KEY, activationCode);
			set.put(COLUMN_USER_DATE_REG, dateReg);

			MySQLAccess.insert(TABLE_USER, set);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}

		return activationCode;
	}

	@Override
	public int activate(String activationKey)
	{
		if (activationKey == null || activationKey.isEmpty())
		{
			throw new IllegalArgumentException("Key is empty");
		}

		try
		{
			final String[] select = { COLUMN_USER_ID, COLUMN_USER_ACTIVATION_KEY };
			final String where = String.format("(%s = ?)", COLUMN_USER_ACTIVATION_KEY);
			final String[] whereArgs = { activationKey };

			int userId = MySQLAccess.select(TABLE_USER, select, where, whereArgs, null, new DataCallback<Integer>()
			{
				@Override
				public Integer onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						int id = set.getInt(COLUMN_USER_ID);
						return id;
					}
					else
					{
						throw new NotAuthorizedException();
					}
				}
			});

			removeActivationKey(userId);
			return userId;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	private static void removeActivationKey(int userId)
	{
		try
		{
			SortedMap<String, String> set = new TreeMap<String, String>();
			set.put(COLUMN_USER_ACTIVATION_KEY, null);

			SortedMap<String, String> where = new TreeMap<String, String>();
			where.put(COLUMN_USER_ID, String.valueOf(userId));

			MySQLAccess.update(TABLE_USER, set, where);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public int login(String login, final String password)
	{
		try
		{
			final String[] select = { COLUMN_USER_ID, COLUMN_USER_HASHPASS, COLUMN_USER_ACTIVATION_KEY };
			final String where = String.format("(%s = ?)", COLUMN_USER_NAME);
			final String[] whereArgs = { login };

			int userId = MySQLAccess.select(TABLE_USER, select, where, whereArgs, null, new DataCallback<Integer>()
			{
				@Override
				public Integer onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						int id = set.getInt(COLUMN_USER_ID);
						String correctSaltedPass = set.getString(COLUMN_USER_HASHPASS);
						String activationCode = set.getString(COLUMN_USER_ACTIVATION_KEY);

						if (activationCode != null)
						{
							throw new NotActivatedException("Not activated");
						}

						if (HashUtils.validatePassword(password, correctSaltedPass))
						{
							return id;
						}
						else
						{
							throw new NotAuthorizedException();
						}
					}
					else
					{
						throw new NotAuthorizedException();
					}
				}
			});
			updateLoginTimestamp(userId);
			return userId;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	static void updateLoginTimestamp(int userId)
	{
		try
		{
			SortedMap<String, String> set = new TreeMap<String, String>();
			set.put(COLUMN_USER_DATE_LOGIN, Utils.formatTimeUTC(new Date()));

			SortedMap<String, String> where = new TreeMap<String, String>();
			where.put(COLUMN_USER_ID, String.valueOf(userId));

			MySQLAccess.update(TABLE_USER, set, where);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Integer getIdByName(String userName)
	{
		try
		{
			final String[] select = { COLUMN_USER_ID };
			final String where = String.format("(%s = ?)", COLUMN_USER_NAME);
			final String[] whereArgs = { userName };

			return MySQLAccess.select(TABLE_USER, select, where, whereArgs, null, new DataCallback<Integer>()
			{
				@Override
				public Integer onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						return set.getInt(COLUMN_USER_ID);
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
	public String getNameById(int userId)
	{
		try
		{
			final String[] select = { COLUMN_USER_NAME };
			final String where = String.format("(%s = ?)", COLUMN_USER_ID);
			final String[] whereArgs = { String.valueOf(userId) };

			return MySQLAccess.select(TABLE_USER, select, where, whereArgs, null, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						return set.getString(COLUMN_USER_NAME);
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
}
