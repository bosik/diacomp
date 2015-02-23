package org.bosik.diacomp.web.backend.common;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.tomcat.jdbc.pool.DataSource;
import org.apache.tomcat.jdbc.pool.PoolProperties;

public class MySQLAccess
{
	public interface DataCallback<T>
	{
		T onData(ResultSet set) throws SQLException;
	}

	private static final String		SQL_DRIVER	= "com.mysql.jdbc.Driver";
	private static final DataSource	datasource;

	public MySQLAccess()
	{
		try
		{
			Class.forName(SQL_DRIVER);
		}
		catch (ClassNotFoundException e)
		{
			throw new RuntimeException(e);
		}
	}

	static
	{
		PoolProperties p = new PoolProperties();

		p.setUrl(Config.get("scheme"));
		p.setDriverClassName(SQL_DRIVER);
		p.setUsername(Config.get("user"));
		p.setPassword(Config.get("password"));
		p.setJmxEnabled(true);
		p.setTestWhileIdle(false);
		p.setTestOnBorrow(true);
		p.setValidationQuery("SELECT 1");
		p.setTestOnReturn(false);
		p.setValidationInterval(30000);
		p.setTimeBetweenEvictionRunsMillis(30000);
		p.setMaxActive(20);
		p.setInitialSize(10);
		p.setMaxWait(10000);
		p.setRemoveAbandonedTimeout(60);
		p.setMinEvictableIdleTimeMillis(30000);
		p.setMinIdle(10);
		p.setMaxIdle(20);
		p.setLogAbandoned(true);
		p.setRemoveAbandoned(true);
		p.setJdbcInterceptors("org.apache.tomcat.jdbc.pool.interceptor.ConnectionState;"
				+ "org.apache.tomcat.jdbc.pool.interceptor.StatementFinalizer");

		datasource = new DataSource();
		datasource.setPoolProperties(p);
	}

	/**
	 * 
	 * @param table
	 *            Table name
	 * @param columns
	 *            A list of which columns to return. Passing null will return all columns, which is inefficient.
	 * @param where
	 *            Selection clause
	 * @param whereArgs
	 *            Arguments for clause
	 * @param order
	 *            Name of column to be ordered by
	 * @param offset
	 *            Index of first row to select
	 * @param limit
	 *            Max number of rows to be selected
	 * @param callback
	 *            What to do with the data
	 * @return
	 * @throws SQLException
	 */
	public static <T> T select(String table, String[] columns, String where, String[] whereArgs, String order,
			int offset, int limit, DataCallback<T> callback) throws SQLException
	{
		Connection connection = datasource.getConnection();
		try
		{
			PreparedStatement statement = Utils.prepareSelectStatement(connection, table, columns, where, whereArgs,
					order, offset, limit);
			// TODO: debug
			System.out.println(statement);
			try
			{
				ResultSet set = statement.executeQuery();
				try
				{
					return callback.onData(set);
				}
				finally
				{
					if (set != null) set.close();
				}
			}
			finally
			{
				if (statement != null) statement.close();
			}
		}
		finally
		{
			if (connection != null) connection.close();
		}
	}

	/**
	 * 
	 * @param table
	 *            Table name
	 * @param columns
	 *            A list of which columns to return. Passing null will return all columns, which is inefficient.
	 * @param where
	 *            Selection clause
	 * @param whereArgs
	 *            Arguments for clause
	 * @param order
	 *            Name of column to be ordered by
	 * @param callback
	 *            What to do with the data
	 * @return
	 * @throws SQLException
	 */
	public static <T> T select(String table, String[] columns, String where, String[] whereArgs, String order,
			DataCallback<T> callback) throws SQLException
	{
		final int offset = -1;
		final int limit = -1;
		return select(table, columns, where, whereArgs, order, offset, limit, callback);
	}

	public static int insert(String table, Map<String, String> values) throws SQLException
	{
		Connection connection = datasource.getConnection();
		try
		{
			PreparedStatement statement = Utils.prepareInsertStatement(connection, table, values);
			// TODO: debug
			System.out.println(statement);
			try
			{
				return statement.executeUpdate();
			}
			finally
			{
				if (statement != null) statement.close();
			}
		}
		finally
		{
			if (connection != null) connection.close();
		}
	}

	public static int update(String table, Map<String, String> set, Map<String, String> where) throws SQLException
	{
		Connection connection = datasource.getConnection();
		try
		{
			PreparedStatement statement = Utils.prepareUpdateStatement(connection, table, set, where);
			// TODO: debug
			System.out.println(statement);
			try
			{
				return statement.executeUpdate();
			}
			finally
			{
				if (statement != null) statement.close();
			}
		}
		finally
		{
			if (connection != null) connection.close();
		}
	}
}

class Utils
{
	public static int count(Iterator<?> iterator)
	{
		int result = 0;
		while (iterator.hasNext())
		{
			result++;
			iterator.next();
		}
		return result;
	}

	public static StringBuilder commaSeparated(Iterator<String> iterator)
	{
		StringBuilder sb = new StringBuilder();

		while (iterator.hasNext())
		{
			sb.append(iterator.next());
			if (iterator.hasNext())
			{
				sb.append(", ");
			}
		}

		return sb;
	}

	public static StringBuilder commaSeparated(String[] array)
	{
		StringBuilder sb = new StringBuilder();

		for (int i = 0; i < array.length; i++)
		{
			sb.append(array[i]);
			if (i < array.length - 1)
			{
				sb.append(", ");
			}
		}

		return sb;
	}

	public static StringBuilder separated(Iterator<String> iterator, String separator)
	{
		StringBuilder sb = new StringBuilder();

		while (iterator.hasNext())
		{
			sb.append(iterator.next());
			sb.append(" = ?");
			if (iterator.hasNext())
			{
				sb.append(separator);
			}
		}

		return sb;
	}

	public static StringBuilder commaSeparatedQuests(int count)
	{
		StringBuilder sb = new StringBuilder();

		for (int i = 0; i < count; i++)
		{
			sb.append("?");
			if (i < (count - 1))
			{
				sb.append(", ");
			}
		}

		return sb;
	}

	public static PreparedStatement prepareSelectStatement(Connection connection, String table, String[] columns,
			String where, String[] whereArgs, String order, int offset, int limit) throws SQLException
	{
		String projection = columns == null ? "*" : Utils.commaSeparated(columns).toString();

		String sql = String.format("SELECT %s FROM %s WHERE %s", projection, table, where);
		if ((order != null) && !order.isEmpty())
		{
			sql += " ORDER BY " + order;
		}

		if (offset >= 0)
		{
			sql += " LIMIT " + offset + ", " + limit;
		}

		PreparedStatement statement = connection.prepareStatement(sql);
		for (int i = 0; i < whereArgs.length; i++)
		{
			statement.setString(i + 1, whereArgs[i]);
		}

		return statement;
	}

	public static PreparedStatement prepareInsertStatement(Connection connection, String table,
			Map<String, String> values) throws SQLException
	{
		// making wildcarded string
		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO " + table + " (");
		sb.append(Utils.commaSeparated(values.keySet().iterator()));
		sb.append(") VALUES (");
		sb.append(Utils.commaSeparatedQuests(Utils.count(values.keySet().iterator())));
		sb.append(")");

		PreparedStatement statement = connection.prepareStatement(sb.toString());

		// filling wildcards
		int i = 1;
		for (Entry<String, String> entry : values.entrySet())
		{
			statement.setString(i++, entry.getValue());
		}
		return statement;
	}

	public static PreparedStatement prepareUpdateStatement(Connection connection, String table,
			Map<String, String> set, Map<String, String> where) throws SQLException
	{
		/**
		 * THINK Requires set.keySet().iterator() and set.entrySet().iterator() return items in the same order
		 */

		// making wildcarded string
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE " + table + " SET ");
		sb.append(Utils.separated(set.keySet().iterator(), ", "));
		sb.append(" WHERE ");
		sb.append(Utils.separated(where.keySet().iterator(), " AND "));

		PreparedStatement statement = connection.prepareStatement(sb.toString());

		// filling wildcards
		int i = 1;
		for (Entry<String, String> entry : set.entrySet())
		{
			statement.setString(i++, entry.getValue());
		}

		for (Entry<String, String> entry : where.entrySet())
		{
			statement.setString(i++, entry.getValue());
		}
		return statement;
	}
}
