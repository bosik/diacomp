package org.bosik.diacomp.web.backend.features.dishbase.function;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.mysql.MySQLAccess;

public class MySQLDishbaseDAO implements DishbaseDAO
{
	private static final MySQLAccess			db			= new MySQLAccess();
	private static final Parser<DishItem>		parser		= new ParserDishItem();
	private static final Serializer<DishItem>	serializer	= new SerializerAdapter<DishItem>(parser);

	private static List<Versioned<DishItem>> parseDishItems(ResultSet resultSet) throws SQLException
	{
		List<Versioned<DishItem>> result = new LinkedList<Versioned<DishItem>>();

		while (resultSet.next())
		{
			String id = resultSet.getString(MySQLAccess.COLUMN_DISHBASE_GUID);
			Date timeStamp = Utils.parseTimeUTC(resultSet.getString(MySQLAccess.COLUMN_DISHBASE_TIMESTAMP));
			int version = resultSet.getInt(MySQLAccess.COLUMN_DISHBASE_VERSION);
			boolean deleted = (resultSet.getInt(MySQLAccess.COLUMN_DISHBASE_DELETED) == 1);
			String content = resultSet.getString(MySQLAccess.COLUMN_DISHBASE_CONTENT);

			Versioned<DishItem> item = new Versioned<DishItem>();
			item.setId(id);
			item.setTimeStamp(timeStamp);
			item.setVersion(version);
			item.setDeleted(deleted);
			item.setData(serializer.read(content));
			//item.setData(content);

			result.add(item);
		}

		return result;
	}

	@Override
	public List<Versioned<DishItem>> findAll(int userId, boolean showRemoved)
	{
		try
		{
			String clause = String.format("(%s = %d)", MySQLAccess.COLUMN_DISHBASE_USER, userId);
			if (!showRemoved)
			{
				clause += String.format(" AND (%s = '%s')", MySQLAccess.COLUMN_DISHBASE_DELETED,
						Utils.formatBooleanInt(false));
			}

			String order = MySQLAccess.COLUMN_DISHBASE_NAMECACHE;

			ResultSet set = db.select(MySQLAccess.TABLE_DISHBASE, clause, order);
			List<Versioned<DishItem>> result = parseDishItems(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<DishItem>> findChanged(int userId, Date since)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s >= '%s')", MySQLAccess.COLUMN_DISHBASE_USER, userId,
					MySQLAccess.COLUMN_DISHBASE_TIMESTAMP, Utils.formatTimeUTC(since));
			String order = MySQLAccess.COLUMN_DISHBASE_NAMECACHE;

			ResultSet set = db.select(MySQLAccess.TABLE_DISHBASE, clause, order);
			List<Versioned<DishItem>> result = parseDishItems(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Versioned<DishItem> findByGuid(int userId, String guid)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s = '%s')", MySQLAccess.COLUMN_DISHBASE_USER, userId,
					MySQLAccess.COLUMN_DISHBASE_GUID, guid);

			ResultSet set = db.select(MySQLAccess.TABLE_DISHBASE, clause, null);
			List<Versioned<DishItem>> result = parseDishItems(set);
			set.close();
			return result.isEmpty() ? null : result.get(0);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<DishItem>> findAny(int userId, String filter)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s = '%s') AND (%s LIKE '%%%s%%')",
					MySQLAccess.COLUMN_DISHBASE_USER, userId, MySQLAccess.COLUMN_DISHBASE_DELETED,
					Utils.formatBooleanInt(false), MySQLAccess.COLUMN_DISHBASE_NAMECACHE, filter);
			String order = MySQLAccess.COLUMN_DISHBASE_NAMECACHE;

			ResultSet set = db.select(MySQLAccess.TABLE_DISHBASE, clause, order);
			List<Versioned<DishItem>> result = parseDishItems(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public void save(int userId, List<Versioned<DishItem>> items)
	{
		try
		{
			for (Versioned<DishItem> item : items)
			{
				final String content = serializer.write(item.getData());
				final String nameCache = item.getData().getName();
				final String timeStamp = Utils.formatTimeUTC(item.getTimeStamp());
				final String version = String.valueOf(item.getVersion());
				final String deleted = Utils.formatBooleanInt(item.isDeleted());

				if (findByGuid(userId, item.getId()) != null)
				{
					// presented, update

					SortedMap<String, String> set = new TreeMap<String, String>();
					set.put(MySQLAccess.COLUMN_DISHBASE_TIMESTAMP, timeStamp);
					set.put(MySQLAccess.COLUMN_DISHBASE_VERSION, version);
					set.put(MySQLAccess.COLUMN_DISHBASE_DELETED, deleted);
					set.put(MySQLAccess.COLUMN_DISHBASE_CONTENT, content);
					set.put(MySQLAccess.COLUMN_DISHBASE_NAMECACHE, nameCache);

					SortedMap<String, String> where = new TreeMap<String, String>();
					where.put(MySQLAccess.COLUMN_DISHBASE_GUID, item.getId());
					where.put(MySQLAccess.COLUMN_DISHBASE_USER, String.valueOf(userId));

					db.update(MySQLAccess.TABLE_DISHBASE, set, where);
				}
				else
				{
					// not presented, insert

					LinkedHashMap<String, String> set = new LinkedHashMap<String, String>();
					set.put(MySQLAccess.COLUMN_DISHBASE_GUID, item.getId());
					set.put(MySQLAccess.COLUMN_DISHBASE_USER, String.valueOf(userId));
					set.put(MySQLAccess.COLUMN_DISHBASE_TIMESTAMP, timeStamp);
					set.put(MySQLAccess.COLUMN_DISHBASE_VERSION, version);
					set.put(MySQLAccess.COLUMN_DISHBASE_DELETED, deleted);
					set.put(MySQLAccess.COLUMN_DISHBASE_CONTENT, content);
					set.put(MySQLAccess.COLUMN_DISHBASE_NAMECACHE, nameCache);

					db.insert(MySQLAccess.TABLE_DISHBASE, set);
				}
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}
}
