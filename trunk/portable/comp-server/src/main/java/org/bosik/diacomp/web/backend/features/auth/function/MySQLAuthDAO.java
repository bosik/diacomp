package org.bosik.diacomp.web.backend.features.auth.function;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.ResultSet;
import java.sql.SQLException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.common.mysql.MySQLAccess;

public class MySQLAuthDAO implements AuthDAO
{
	private final MySQLAccess		db	= new MySQLAccess();
	private static MessageDigest	md5digest;

	{
		try
		{
			md5digest = MessageDigest.getInstance("MD5");
		}
		catch (NoSuchAlgorithmException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public int login(String login, String pass)
	{
		try
		{
			String hash = md5(pass);
			String clause = String.format("(%s = ?) AND (%s = ?)", MySQLAccess.COLUMN_USER_LOGIN,
					MySQLAccess.COLUMN_USER_HASHPASS);

			ResultSet set = db.select(MySQLAccess.TABLE_USER, clause, null, login, hash);

			if (set.next())
			{
				String s_id = set.getString(MySQLAccess.COLUMN_USER_ID);
				set.close();
				int id = Integer.parseInt(s_id);

				// TODO: update DateLogin field

				return id;
			}
			else
			{
				set.close();
				throw new NotAuthorizedException();
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	public static String md5(String s)
	{
		try
		{
			byte[] input = md5digest.digest(s.getBytes("UTF-8"));
			StringBuffer sb = new StringBuffer();
			for (int i = 0; i < input.length; ++i)
			{
				sb.append(Integer.toHexString((input[i] & 0xFF) | 0x100).substring(1, 3));
			}
			return sb.toString();
		}
		catch (UnsupportedEncodingException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Integer getIdByName(String userName)
	{
		try
		{
			String clause = String.format("(%s = ?)", MySQLAccess.COLUMN_USER_LOGIN);

			ResultSet set = db.select(MySQLAccess.TABLE_USER, clause, null, userName);

			if (set.next())
			{
				String s_id = set.getString(MySQLAccess.COLUMN_USER_ID);
				set.close();
				return Integer.parseInt(s_id);
			}
			else
			{
				set.close();
				return null;
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}
}
