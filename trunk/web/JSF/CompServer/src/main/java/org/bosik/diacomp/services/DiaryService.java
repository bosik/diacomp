package org.bosik.diacomp.services;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import org.bosik.diacomp.MySQLAccess;
import org.bosik.diacomp.persistence.common.Versioned;

public class DiaryService
{
	private final MySQLAccess	db	= new MySQLAccess();

	public List<Versioned<String>> findAll(int userId)
	{
		try
		{
			String clause = String.format("%s = %d", MySQLAccess.COLUMN_DIARY_USER, userId);
			ResultSet set = db.select(MySQLAccess.TABLE_DIARY, clause);
			List<Versioned<String>> result = parseDiaryRecords(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	private static List<Versioned<String>> parseDiaryRecords(ResultSet resultSet) throws SQLException
	{
		// Parser<DiaryRecord> parser = new ParserDiaryRecord();
		// Serializer<DiaryRecord> serializer = new SerializerAdapter<DiaryRecord>(parser);

		List<Versioned<String>> result = new LinkedList<Versioned<String>>();

		while (resultSet.next())
		{
			String id = resultSet.getString(MySQLAccess.COLUMN_DIARY_GUID);
			Date timeStamp = resultSet.getDate(MySQLAccess.COLUMN_DIARY_TIMESTAMP);
			int version = resultSet.getInt(MySQLAccess.COLUMN_DIARY_VERSION);
			boolean deleted = (resultSet.getInt(MySQLAccess.COLUMN_DIARY_DELETED) == 1);
			String content = resultSet.getString(MySQLAccess.COLUMN_DIARY_CONTENT);

			Versioned<String> item = new Versioned<String>();
			item.setId(id);
			item.setTimeStamp(timeStamp);
			item.setVersion(version);
			item.setDeleted(deleted);
			item.setData(content);

			result.add(item);
		}

		return result;
	}
}
