package org.bosik.diacomp.web.backend.common.mysql;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.SortedMap;

public class MySQLAccess
{
	private static Connection	connection;

	private static final String	SQL_DRIVER					= "com.mysql.jdbc.Driver";
	private static final String	SCHEMA						= "compensation";
	private static final String	USERNAME					= "root";
	private static final String	PASSWORD					= "root";
	private static final String	connectionString			= String.format(
																	"jdbc:mysql://127.0.0.1:3306/%s?user=%s&password=%s&autoReconnect=true&failOverReadOnly=false&maxReconnects=10",
																	SCHEMA, USERNAME, PASSWORD);

	// ======================================= User table =======================================

	public static final String	TABLE_USER					= "user";
	public static final String	COLUMN_USER_ID				= "ID";
	public static final String	COLUMN_USER_LOGIN			= "Login";
	public static final String	COLUMN_USER_HASHPASS		= "HashPass";
	public static final String	COLUMN_USER_DATE_REG		= "DateReg";
	public static final String	COLUMN_USER_DATE_LOGIN		= "DateLogin";

	// ======================================= Diary table =======================================

	public static final String	TABLE_DIARY					= "diary2";
	public static final String	COLUMN_DIARY_GUID			= "_GUID";
	public static final String	COLUMN_DIARY_USER			= "_UserID";
	public static final String	COLUMN_DIARY_TIMESTAMP		= "_TimeStamp";
	public static final String	COLUMN_DIARY_VERSION		= "_Version";
	public static final String	COLUMN_DIARY_DELETED		= "_Deleted";
	public static final String	COLUMN_DIARY_CONTENT		= "_Content";
	public static final String	COLUMN_DIARY_TIMECACHE		= "_TimeCache";

	// ======================================= Food table =======================================

	public static final String	TABLE_FOODBASE				= "foodbase2";
	public static final String	COLUMN_FOODBASE_GUID		= "_GUID";
	public static final String	COLUMN_FOODBASE_USER		= "_UserID";
	public static final String	COLUMN_FOODBASE_TIMESTAMP	= "_TimeStamp";
	public static final String	COLUMN_FOODBASE_VERSION		= "_Version";
	public static final String	COLUMN_FOODBASE_DELETED		= "_Deleted";
	public static final String	COLUMN_FOODBASE_CONTENT		= "_Content";
	public static final String	COLUMN_FOODBASE_NAMECACHE	= "_NameCache";

	// ======================================= Dish table =======================================

	public static final String	TABLE_DISHBASE				= "dishbase2";
	public static final String	COLUMN_DISHBASE_GUID		= "_GUID";
	public static final String	COLUMN_DISHBASE_USER		= "_UserID";
	public static final String	COLUMN_DISHBASE_TIMESTAMP	= "_TimeStamp";
	public static final String	COLUMN_DISHBASE_VERSION		= "_Version";
	public static final String	COLUMN_DISHBASE_DELETED		= "_Deleted";
	public static final String	COLUMN_DISHBASE_CONTENT		= "_Content";
	public static final String	COLUMN_DISHBASE_NAMECACHE	= "_NameCache";

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

	private static void connect() throws SQLException
	{
		if (connection == null)
		{
			connection = DriverManager.getConnection(connectionString);
		}
	}

	// the resource is returned to invoker
	public ResultSet select(String table, String clause, String order, String... params) throws SQLException
	{
		connect();

		String sql = String.format("SELECT * FROM %s WHERE %s", table, clause);
		if ((order != null) && !order.isEmpty())
		{
			sql += " ORDER BY " + order;
		}

		PreparedStatement preparedStatement = connection.prepareStatement(sql);
		for (int i = 0; i < params.length; i++)
		{
			preparedStatement.setString(i + 1, params[i]);
		}

		// Don't close prepared statement!
		return preparedStatement.executeQuery();
	}

	public int insert(String table, LinkedHashMap<String, String> set)
	{
		try
		{
			connect();

			// making wildcarded string

			StringBuilder sb = new StringBuilder();
			sb.append("INSERT INTO " + table + " (");
			sb.append(Utils.commaSeparated(set.keySet().iterator()));
			sb.append(") VALUES (");
			sb.append(Utils.commaSeparatedQuests(Utils.count(set.keySet().iterator())));
			sb.append(")");

			PreparedStatement preparedStatement = connection.prepareStatement(sb.toString());
			System.out.println(sb);

			// filling wildcards

			int i = 1;
			for (Entry<String, String> entry : set.entrySet())
			{
				preparedStatement.setString(i++, entry.getValue());
			}

			// go

			return preparedStatement.executeUpdate();
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	public int update(String table, SortedMap<String, String> set, SortedMap<String, String> where) throws SQLException
	{
		try
		{
			connect();

			// making wildcarded string
			StringBuilder sb = new StringBuilder();
			sb.append("UPDATE " + table + " SET ");
			sb.append(Utils.separated(set.keySet().iterator(), ", "));
			sb.append(" WHERE ");
			sb.append(Utils.separated(where.keySet().iterator(), " AND "));

			System.out.println(sb);

			PreparedStatement preparedStatement = connection.prepareStatement(sb.toString());

			// filling wildcards

			int i = 1;
			// statement.setString(i++, table);

			for (Entry<String, String> entry : set.entrySet())
			{
				// statement.setString(i++, entry.getKey());
				preparedStatement.setString(i++, entry.getValue());
			}

			for (Entry<String, String> entry : where.entrySet())
			{
				// statement.setString(i++, entry.getKey());
				preparedStatement.setString(i++, entry.getValue());
			}

			// go

			return preparedStatement.executeUpdate();
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	// private void example()
	// {
	// try
	// {
	// // Setup the connection with the DB
	// connection = DriverManager.getConnection(connectionString);
	//
	// // Statements allow to issue SQL queries to the database
	// statement = connection.createStatement();
	// statement.executeQuery("select * from " + TABLE_DIARY);
	//
	// //
	// ===============================================================================================
	//
	// // PreparedStatements can use variables and are more efficient
	// preparedStatement = connection
	// .prepareStatement("insert into FEEDBACK.COMMENTS values (default, ?, ?, ?, ? , ?, ?)");
	// // "myuser, webpage, datum, summary, COMMENTS from FEEDBACK.COMMENTS");
	// // Parameters start with 1
	// preparedStatement.setString(1, "Test");
	// preparedStatement.setString(2, "TestEmail");
	// preparedStatement.setString(3, "TestWebpage");
	// preparedStatement.setDate(4, new java.sql.Date(2009, 12, 11));
	// preparedStatement.setString(5, "TestSummary");
	// preparedStatement.setString(6, "TestComment");
	// preparedStatement.executeUpdate();
	//
	// //
	// ===============================================================================================
	//
	// preparedStatement = connection
	// .prepareStatement("SELECT myuser, webpage, datum, summary, COMMENTS from FEEDBACK.COMMENTS");
	// resultSet = preparedStatement.executeQuery();
	// // parseDiaryRecords(resultSet);
	//
	// //
	// ===============================================================================================
	//
	// // Remove again the insert comment
	// preparedStatement =
	// connection.prepareStatement("delete from FEEDBACK.COMMENTS where myuser= ? ; ");
	// preparedStatement.setString(1, "Test");
	// preparedStatement.executeUpdate();
	//
	// //
	// ===============================================================================================
	//
	// resultSet = statement.executeQuery("select * from diary");
	// System.out.println("The columns in the table are: ");
	//
	// System.out.println("Table: " + resultSet.getMetaData().getTableName(1));
	// for (int i = 1; i <= resultSet.getMetaData().getColumnCount(); i++)
	// {
	// System.out.println("Column " + i + " " + resultSet.getMetaData().getColumnName(i));
	// }
	// }
	// catch (SQLException e)
	// {
	// throw new RuntimeException(e);
	// }
	// finally
	// {
	// //close();
	// }
	// }
}