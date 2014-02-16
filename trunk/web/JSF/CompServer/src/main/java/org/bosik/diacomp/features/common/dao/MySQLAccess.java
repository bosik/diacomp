package org.bosik.diacomp.features.common.dao;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.SortedMap;

public class MySQLAccess
{
	private static final String	SQL_DRIVER				= "com.mysql.jdbc.Driver";
	private static final String	SCHEMA					= "compensation";
	private static final String	USERNAME				= "root";
	private static final String	PASSWORD				= "root";
	private static final String	connectionString		= String.format(
																"jdbc:mysql://127.0.0.1:3306/%s?user=%s&password=%s",
																SCHEMA, USERNAME, PASSWORD);

	// ======================================= User table =======================================

	public static final String	TABLE_USER				= "user";
	public static final String	COLUMN_USER_ID			= "ID";
	public static final String	COLUMN_USER_LOGIN		= "Login";
	public static final String	COLUMN_USER_HASHPASS	= "HashPass";
	public static final String	COLUMN_USER_DATE_REG	= "DateReg";
	public static final String	COLUMN_USER_DATE_LOGIN	= "DateLogin";

	// ======================================= Diary table =======================================

	public static final String	TABLE_DIARY				= "diary2";
	public static final String	COLUMN_DIARY_GUID		= "_GUID";
	public static final String	COLUMN_DIARY_USER		= "_UserID";
	public static final String	COLUMN_DIARY_TIMESTAMP	= "_TimeStamp";
	public static final String	COLUMN_DIARY_VERSION	= "_Version";
	public static final String	COLUMN_DIARY_DELETED	= "_Deleted";
	public static final String	COLUMN_DIARY_CONTENT	= "_Content";
	public static final String	COLUMN_DIARY_TIMECACHE	= "_TimeCache";

	private Connection			connect					= null;
	private Statement			statement				= null;
	private PreparedStatement	preparedStatement		= null;
	private ResultSet			resultSet				= null;

	private static void init()
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

	public MySQLAccess()
	{
		init();
	}

	@SuppressWarnings("resource")
	// the resource is returned to invoker
	public ResultSet select(String table, String clause, String... params) throws SQLException
	{
		Connection connect = null;
		PreparedStatement statement = null;
		try
		{
			connect = DriverManager.getConnection(connectionString);

			statement = connect.prepareStatement(String.format("SELECT * FROM %s WHERE %s", table, clause));
			for (int i = 0; i < params.length; i++)
			{
				statement.setString(i + 1, params[i]);
			}

			return statement.executeQuery();
		}
		finally
		{
			close();
		}
	}

	private static int count(Iterator iterator)
	{
		int result = 0;
		while (iterator.hasNext())
		{
			result++;
			iterator.next();
		}
		return result;
	}

	private static StringBuilder commaSeparated(Iterator<String> iterator)
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

	private static StringBuilder separated(Iterator<String> iterator, String separator)
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

	private static StringBuilder commaSeparatedQuests(int count)
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

	public int insert(String table, LinkedHashMap<String, String> set)
	{
		Connection connect = null;
		PreparedStatement statement = null;
		try
		{
			connect = DriverManager.getConnection(connectionString);

			// making wildcarded string

			StringBuilder sb = new StringBuilder();
			sb.append("INSERT INTO " + table + " (");
			sb.append(commaSeparated(set.keySet().iterator()));
			sb.append(") VALUES (");
			sb.append(commaSeparatedQuests(count(set.keySet().iterator())));
			sb.append(")");

			statement = connect.prepareStatement(sb.toString());
			System.out.println(sb);

			// filling wildcards

			int i = 1;
			for (Entry<String, String> entry : set.entrySet())
			{
				statement.setString(i++, entry.getValue());
			}

			// go

			return statement.executeUpdate();
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
		finally
		{
			close();
		}

	}

	public int update(String table, SortedMap<String, String> set, SortedMap<String, String> where) throws SQLException
	{
		Connection connect = null;
		PreparedStatement statement = null;
		try
		{
			connect = DriverManager.getConnection(connectionString);

			// making wildcarded string
			StringBuilder sb = new StringBuilder();
			sb.append("UPDATE " + table + " SET ");
			sb.append(separated(set.keySet().iterator(), ", "));
			sb.append(" WHERE ");
			sb.append(separated(where.keySet().iterator(), " AND "));

			System.out.println(sb);

			statement = connect.prepareStatement(sb.toString());

			// filling wildcards

			int i = 1;
			// statement.setString(i++, table);

			for (Entry<String, String> entry : set.entrySet())
			{
				// statement.setString(i++, entry.getKey());
				statement.setString(i++, entry.getValue());
			}

			for (Entry<String, String> entry : where.entrySet())
			{
				// statement.setString(i++, entry.getKey());
				statement.setString(i++, entry.getValue());
			}

			// go

			return statement.executeUpdate();
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
		finally
		{
			close();
		}
	}

	private void example()
	{
		try
		{
			// Setup the connection with the DB
			connect = DriverManager.getConnection(connectionString);

			// Statements allow to issue SQL queries to the database
			statement = connect.createStatement();
			statement.executeQuery("select * from " + TABLE_DIARY);

			// ===============================================================================================

			// PreparedStatements can use variables and are more efficient
			preparedStatement = connect
					.prepareStatement("insert into FEEDBACK.COMMENTS values (default, ?, ?, ?, ? , ?, ?)");
			// "myuser, webpage, datum, summary, COMMENTS from FEEDBACK.COMMENTS");
			// Parameters start with 1
			preparedStatement.setString(1, "Test");
			preparedStatement.setString(2, "TestEmail");
			preparedStatement.setString(3, "TestWebpage");
			preparedStatement.setDate(4, new java.sql.Date(2009, 12, 11));
			preparedStatement.setString(5, "TestSummary");
			preparedStatement.setString(6, "TestComment");
			preparedStatement.executeUpdate();

			// ===============================================================================================

			preparedStatement = connect
					.prepareStatement("SELECT myuser, webpage, datum, summary, COMMENTS from FEEDBACK.COMMENTS");
			resultSet = preparedStatement.executeQuery();
			// parseDiaryRecords(resultSet);

			// ===============================================================================================

			// Remove again the insert comment
			preparedStatement = connect.prepareStatement("delete from FEEDBACK.COMMENTS where myuser= ? ; ");
			preparedStatement.setString(1, "Test");
			preparedStatement.executeUpdate();

			// ===============================================================================================

			resultSet = statement.executeQuery("select * from diary");
			System.out.println("The columns in the table are: ");

			System.out.println("Table: " + resultSet.getMetaData().getTableName(1));
			for (int i = 1; i <= resultSet.getMetaData().getColumnCount(); i++)
			{
				System.out.println("Column " + i + " " + resultSet.getMetaData().getColumnName(i));
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
		finally
		{
			close();
		}
	}

	private void close()
	{
		try
		{
			if (resultSet != null)
			{
				resultSet.close();
			}

			if (statement != null)
			{
				statement.close();
			}

			if (connect != null)
			{
				connect.close();
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}
}
