package org.bosik.diacomp;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import org.bosik.compensation.bo.diary.DiaryRecord;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.serializers.Parser;
import org.bosik.compensation.persistence.serializers.ParserDiaryRecord;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.persistence.serializers.utils.SerializerAdapter;

public class MySQLAccess
{
	private static final String	SQL_DRIVER				= "com.mysql.jdbc.Driver";
	private static final String	SCHEMA					= "compensation";
	private static final String	USERNAME				= "root";
	private static final String	PASSWORD				= "root";
	private static final String	connectionString		= String.format(
																"jdbc:mysql://127.0.0.1:3306/%s?user=%s&password=%s",
																SCHEMA, USERNAME, PASSWORD);

	// ======================================= Diary table =======================================

	public static final String	TABLE_DIARY				= "diary2";
	public static final String	COLUMN_DIARY_GUID		= "_GUID";
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

	public ResultSet select(String table, String clause) throws SQLException
	{
		try
		{
			connect = DriverManager.getConnection(connectionString);

			// String sql = String.format("SELECT * FROM %s WHERE %s", table, clause);
			// statement = connect.createStatement();
			// return statement.executeQuery(sql);

			String sql = String.format("SELECT * FROM ? WHERE ?");
			preparedStatement = connect.prepareStatement(sql);
			preparedStatement.setString(1, table);
			preparedStatement.setString(2, clause);
			return preparedStatement.executeQuery();
		}
		finally
		{
			close();
		}
	}

	public void readDataBase()
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
			parseDiaryRecords(resultSet);

			// ===============================================================================================

			// Remove again the insert comment
			preparedStatement = connect.prepareStatement("delete from FEEDBACK.COMMENTS where myuser= ? ; ");
			preparedStatement.setString(1, "Test");
			preparedStatement.executeUpdate();

			resultSet = statement.executeQuery("select * from diary");
			writeMetaData(resultSet);

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

	private void writeMetaData(ResultSet resultSet) throws SQLException
	{
		// Now get some metadata from the database
		// Result set get the result of the SQL query

		System.out.println("The columns in the table are: ");

		System.out.println("Table: " + resultSet.getMetaData().getTableName(1));
		for (int i = 1; i <= resultSet.getMetaData().getColumnCount(); i++)
		{
			System.out.println("Column " + i + " " + resultSet.getMetaData().getColumnName(i));
		}
	}

	private List<Versioned<DiaryRecord>> parseDiaryRecords(ResultSet resultSet) throws SQLException
	{
		Parser<DiaryRecord> parser = new ParserDiaryRecord();
		Serializer<DiaryRecord> serializer = new SerializerAdapter<DiaryRecord>(parser);

		List<Versioned<DiaryRecord>> result = new LinkedList<Versioned<DiaryRecord>>();

		while (resultSet.next())
		{
			String id = resultSet.getString(COLUMN_DIARY_GUID);
			Date timeStamp = resultSet.getDate(COLUMN_DIARY_TIMESTAMP);
			int version = resultSet.getInt(COLUMN_DIARY_VERSION);
			boolean deleted = (resultSet.getInt(COLUMN_DIARY_DELETED) == 1);
			String content = resultSet.getString(COLUMN_DIARY_CONTENT);

			Versioned<DiaryRecord> item = new Versioned<DiaryRecord>();
			item.setId(id);
			item.setTimeStamp(timeStamp);
			item.setVersion(version);
			item.setDeleted(deleted);
			item.setData(serializer.read(content));

			result.add(item);
		}

		return result;
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
