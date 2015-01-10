package org.bosik.diacomp.web.backend.common.mysql;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;
import java.util.Map.Entry;
import org.bosik.diacomp.web.backend.common.Config;

public class MySQLAccess
{
	private static Connection	connection;

	private static final String	SQL_DRIVER	= "com.mysql.jdbc.Driver";

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
			String connectionString = Config.get("connection");
			connection = DriverManager.getConnection(connectionString);
		}
	}

	// the resource is returned to invoker
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
	 * @return
	 * @throws SQLException
	 */
	@SuppressWarnings("static-method")
	public ResultSet select(String table, String[] columns, String where, String[] whereArgs, String order, int offset,
			int limit) throws SQLException
	{
		connect();

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

		PreparedStatement preparedStatement = connection.prepareStatement(sql);
		for (int i = 0; i < whereArgs.length; i++)
		{
			preparedStatement.setString(i + 1, whereArgs[i]);
		}

		// Don't close prepared statement!
		return preparedStatement.executeQuery();
	}

	/**
	 * Selects all fields
	 * 
	 * @param table
	 * @param where
	 * @param order
	 * @param whereArgs
	 * @return
	 * @throws SQLException
	 */
	public ResultSet select(String table, String where, String order, String[] whereArgs) throws SQLException
	{
		return select(table, null, where, whereArgs, order, -1, -1);
	}

	public ResultSet select(String table, String[] columns, String where, String[] whereArgs, String order)
			throws SQLException
	{
		return select(table, columns, where, whereArgs, order, -1, -1);
	}

	@SuppressWarnings("static-method")
	public int insert(String table, Map<String, String> values) throws SQLException
	{
		connect();

		// making wildcarded string

		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO " + table + " (");
		sb.append(Utils.commaSeparated(values.keySet().iterator()));
		sb.append(") VALUES (");
		sb.append(Utils.commaSeparatedQuests(Utils.count(values.keySet().iterator())));
		sb.append(")");

		PreparedStatement preparedStatement = connection.prepareStatement(sb.toString());
		// TODO: debug only
		System.out.println(sb);

		// filling wildcards

		int i = 1;
		for (Entry<String, String> entry : values.entrySet())
		{
			preparedStatement.setString(i++, entry.getValue());
		}

		// go

		return preparedStatement.executeUpdate();
	}

	@SuppressWarnings("static-method")
	public int update(String table, Map<String, String> set, Map<String, String> where) throws SQLException
	{
		connect();

		// making wildcarded string
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE " + table + " SET ");
		sb.append(Utils.separated(set.keySet().iterator(), ", "));
		sb.append(" WHERE ");
		sb.append(Utils.separated(where.keySet().iterator(), " AND "));

		// TODO: debug only
		System.out.println(sb);

		PreparedStatement preparedStatement = connection.prepareStatement(sb.toString());

		// filling wildcards

		int i = 1;
		// statement.setString(i++, table);

		for (Entry<String, String> entry : set.entrySet())
		{
			// statement.setString(i++, entry.getKey());
			preparedStatement.setString(i++, entry.getValue());
			// TODO: debug only
			System.out.println("UPDATE: " + entry.getKey() + " = " + entry.getValue());
		}

		for (Entry<String, String> entry : where.entrySet())
		{
			// statement.setString(i++, entry.getKey());
			preparedStatement.setString(i++, entry.getValue());
		}

		// go

		return preparedStatement.executeUpdate();
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
