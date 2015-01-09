package org.bosik.diacomp.web.backend.features.base.dish.function;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.mysql.MySQLAccess;

public class MySQLDishbaseDAO implements DishbaseDAO
{
	// Dishbase table
	private static final String					TABLE_DISHBASE				= "dishbase2";
	private static final String					COLUMN_DISHBASE_GUID		= "_GUID";
	private static final String					COLUMN_DISHBASE_USER		= "_UserID";
	private static final String					COLUMN_DISHBASE_TIMESTAMP	= "_TimeStamp";
	private static final String					COLUMN_DISHBASE_VERSION		= "_Version";
	private static final String					COLUMN_DISHBASE_DELETED		= "_Deleted";
	private static final String					COLUMN_DISHBASE_CONTENT		= "_Content";
	private static final String					COLUMN_DISHBASE_NAMECACHE	= "_NameCache";

	// Dishbase hash table
	private static final String					TABLE_DISHBASE_HASH			= "dishbase_hash";
	private static final String					COLUMN_DISHBASE_HASH_USER	= "_UserID";
	private static final String					COLUMN_DISHBASE_HASH_GUID	= "_GUID";
	private static final String					COLUMN_DISHBASE_HASH_HASH	= "_Hash";

	private static final MySQLAccess			db							= new MySQLAccess();
	private static final Parser<DishItem>		parser						= new ParserDishItem();
	private static final Serializer<DishItem>	serializer					= new SerializerAdapter<DishItem>(parser);

	private static List<Versioned<DishItem>> parseDishItems(ResultSet resultSet) throws SQLException
	{
		List<Versioned<DishItem>> result = new LinkedList<Versioned<DishItem>>();

		while (resultSet.next())
		{
			String id = resultSet.getString(COLUMN_DISHBASE_GUID);
			Date timeStamp = Utils.parseTimeUTC(resultSet.getString(COLUMN_DISHBASE_TIMESTAMP));
			int version = resultSet.getInt(COLUMN_DISHBASE_VERSION);
			boolean deleted = (resultSet.getInt(COLUMN_DISHBASE_DELETED) == 1);
			String content = resultSet.getString(COLUMN_DISHBASE_CONTENT);

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
	public List<Versioned<DishItem>> findAll(int userId, boolean includeRemoved)
	{
		try
		{
			String clause = String.format("(%s = %d)", COLUMN_DISHBASE_USER, userId);
			if (!includeRemoved)
			{
				clause += String.format(" AND (%s = '%s')", COLUMN_DISHBASE_DELETED, Utils.formatBooleanInt(false));
			}

			String order = COLUMN_DISHBASE_NAMECACHE;

			ResultSet set = db.select(TABLE_DISHBASE, clause, order);
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
			String clause = String.format("(%s = %d) AND (%s >= '%s')", COLUMN_DISHBASE_USER, userId,
					COLUMN_DISHBASE_TIMESTAMP, Utils.formatTimeUTC(since));
			String order = COLUMN_DISHBASE_NAMECACHE;

			ResultSet set = db.select(TABLE_DISHBASE, clause, order);
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
	public Versioned<DishItem> findById(int userId, String guid)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s = '%s')", COLUMN_DISHBASE_USER, userId,
					COLUMN_DISHBASE_GUID, guid);

			ResultSet set = db.select(TABLE_DISHBASE, clause, null);
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
	public List<Versioned<DishItem>> findByIdPrefix(int userId, String prefix)
	{
		if (prefix.length() != ObjectService.ID_PREFIX_SIZE)
		{
			throw new IllegalArgumentException(String.format("Invalid prefix length, expected %d chars, but %d found",
					ObjectService.ID_PREFIX_SIZE, prefix.length()));
		}

		try
		{
			String clause = String.format("(%s = %d) AND (%s LIKE '%s%%')", COLUMN_DISHBASE_USER, userId,
					COLUMN_DISHBASE_GUID, prefix);

			String order = COLUMN_DISHBASE_NAMECACHE;

			ResultSet set = db.select(TABLE_DISHBASE, clause, order);
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
	public List<Versioned<DishItem>> findAny(int userId, String filter)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s = '%s') AND (%s LIKE '%%%s%%')", COLUMN_DISHBASE_USER,
					userId, COLUMN_DISHBASE_DELETED, Utils.formatBooleanInt(false), COLUMN_DISHBASE_NAMECACHE, filter);
			String order = COLUMN_DISHBASE_NAMECACHE;

			ResultSet set = db.select(TABLE_DISHBASE, clause, order);
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

				if (findById(userId, item.getId()) != null)
				{
					// presented, update

					SortedMap<String, String> set = new TreeMap<String, String>();
					set.put(COLUMN_DISHBASE_TIMESTAMP, timeStamp);
					set.put(COLUMN_DISHBASE_VERSION, version);
					set.put(COLUMN_DISHBASE_DELETED, deleted);
					set.put(COLUMN_DISHBASE_CONTENT, content);
					set.put(COLUMN_DISHBASE_NAMECACHE, nameCache);

					SortedMap<String, String> where = new TreeMap<String, String>();
					where.put(COLUMN_DISHBASE_GUID, item.getId());
					where.put(COLUMN_DISHBASE_USER, String.valueOf(userId));

					db.update(TABLE_DISHBASE, set, where);
				}
				else
				{
					// not presented, insert

					LinkedHashMap<String, String> set = new LinkedHashMap<String, String>();
					set.put(COLUMN_DISHBASE_GUID, item.getId());
					set.put(COLUMN_DISHBASE_USER, String.valueOf(userId));
					set.put(COLUMN_DISHBASE_TIMESTAMP, timeStamp);
					set.put(COLUMN_DISHBASE_VERSION, version);
					set.put(COLUMN_DISHBASE_DELETED, deleted);
					set.put(COLUMN_DISHBASE_CONTENT, content);
					set.put(COLUMN_DISHBASE_NAMECACHE, nameCache);

					db.insert(TABLE_DISHBASE, set);
				}
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Versioned<DishItem> findOne(int userId, String exactName)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s = '%s')", COLUMN_DISHBASE_USER, userId,
					COLUMN_DISHBASE_NAMECACHE, exactName);

			ResultSet set = db.select(TABLE_DISHBASE, clause, null);
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
	public void delete(int userId, String id)
	{
		try
		{
			SortedMap<String, String> set = new TreeMap<String, String>();
			set.put(COLUMN_DISHBASE_DELETED, Utils.formatBooleanInt(true));

			SortedMap<String, String> where = new TreeMap<String, String>();
			where.put(COLUMN_DISHBASE_GUID, id);
			where.put(COLUMN_DISHBASE_USER, String.valueOf(userId));

			db.update(TABLE_DISHBASE, set, where);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public String getHash(int userId, String prefix)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s = '%s')", COLUMN_DISHBASE_HASH_USER, userId,
					COLUMN_DISHBASE_HASH_GUID, prefix);

			ResultSet set = db.select(TABLE_DISHBASE_HASH, clause, null);

			String hash = "";

			if (set.next())
			{
				hash = set.getString(COLUMN_DISHBASE_HASH_HASH);
			}

			set.close();
			return hash;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Map<String, String> getHashChildren(int userId, String prefix)
	{
		try
		{
			String clause = String.format("(%s = %d) AND (%s LIKE '%s')", COLUMN_DISHBASE_HASH_USER, userId,
					COLUMN_DISHBASE_HASH_GUID, prefix + "_");
			ResultSet set = db.select(TABLE_DISHBASE_HASH, clause, null);

			Map<String, String> result = new HashMap<String, String>();
			while (set.next())
			{
				String id = set.getString(COLUMN_DISHBASE_HASH_GUID);
				String hash = set.getString(COLUMN_DISHBASE_HASH_HASH);
				result.put(id, hash);
			}

			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}
}
