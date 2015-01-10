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
	private static final String			TABLE_USER				= "user";
	private static final String			COLUMN_USER_ID			= "ID";
	private static final String			COLUMN_USER_LOGIN		= "Login";
	private static final String			COLUMN_USER_HASHPASS	= "HashPass";
	private static final String			COLUMN_USER_DATE_REG	= "DateReg";
	private static final String			COLUMN_USER_DATE_LOGIN	= "DateLogin";

	private static final MySQLAccess	db						= new MySQLAccess();
	private static MessageDigest		md5digest;

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

			final String[] select = { COLUMN_USER_ID };
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_USER_LOGIN, COLUMN_USER_HASHPASS);
			final String[] whereArgs = { login, hash };

			ResultSet set = db.select(TABLE_USER, select, where, whereArgs, null);

			if (set.next())
			{
				int id = set.getInt(COLUMN_USER_ID);
				set.close();

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
			final String[] select = { COLUMN_USER_ID };
			final String where = String.format("(%s = ?)", COLUMN_USER_LOGIN);
			final String[] whereArgs = { userName };

			ResultSet set = db.select(TABLE_USER, select, where, whereArgs, null);

			if (set.next())
			{
				int id = set.getInt(COLUMN_USER_ID);
				set.close();
				return id;
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
