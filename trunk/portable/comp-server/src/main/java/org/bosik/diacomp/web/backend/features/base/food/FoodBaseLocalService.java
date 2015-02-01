package org.bosik.diacomp.web.backend.features.base.food;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.services.sync.HashUtils;
import org.bosik.diacomp.core.services.sync.MemoryMerkleTree;
import org.bosik.diacomp.core.services.sync.MerkleTree;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class FoodBaseLocalService implements FoodBaseService
{
	// Foodbase table
	private static final String					TABLE_FOODBASE				= "foodbase2";
	private static final String					COLUMN_FOODBASE_GUID		= "_GUID";
	private static final String					COLUMN_FOODBASE_USER		= "_UserID";
	private static final String					COLUMN_FOODBASE_TIMESTAMP	= "_TimeStamp";
	private static final String					COLUMN_FOODBASE_HASH		= "_Hash";
	private static final String					COLUMN_FOODBASE_VERSION		= "_Version";
	private static final String					COLUMN_FOODBASE_DELETED		= "_Deleted";
	private static final String					COLUMN_FOODBASE_CONTENT		= "_Content";
	private static final String					COLUMN_FOODBASE_NAMECACHE	= "_NameCache";

	// Foodbase hash table
	private static final String					TABLE_FOODBASE_HASH			= "foodbase_hash";
	private static final String					COLUMN_FOODBASE_HASH_USER	= "_UserID";
	private static final String					COLUMN_FOODBASE_HASH_GUID	= "_GUID";
	private static final String					COLUMN_FOODBASE_HASH_HASH	= "_Hash";

	private static final MySQLAccess			db							= new MySQLAccess();
	private static final Parser<FoodItem>		parser						= new ParserFoodItem();
	private static final Serializer<FoodItem>	serializer					= new SerializerAdapter<FoodItem>(parser);

	@Autowired
	private UserInfoService						userInfoService;

	protected int getCurrentUserId()
	{
		return userInfoService.getCurrentUserId();
	}

	private static List<Versioned<FoodItem>> parseItems(ResultSet resultSet, int limit) throws SQLException
	{
		List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();

		while (resultSet.next())
		{
			String id = resultSet.getString(COLUMN_FOODBASE_GUID);
			Date timeStamp = Utils.parseTimeUTC(resultSet.getString(COLUMN_FOODBASE_TIMESTAMP));
			String hash = resultSet.getString(COLUMN_FOODBASE_HASH);
			int version = resultSet.getInt(COLUMN_FOODBASE_VERSION);
			boolean deleted = (resultSet.getInt(COLUMN_FOODBASE_DELETED) == 1);
			String content = resultSet.getString(COLUMN_FOODBASE_CONTENT);

			Versioned<FoodItem> item = new Versioned<FoodItem>();
			item.setId(id);
			item.setTimeStamp(timeStamp);
			item.setHash(hash);
			item.setVersion(version);
			item.setDeleted(deleted);
			item.setData(serializer.read(content));

			result.add(item);

			if (limit > 0 && result.size() > limit)
			{
				throw new TooManyItemsException("Too many items");
			}
		}

		return result;
	}

	private static List<Versioned<FoodItem>> parseItems(ResultSet resultSet) throws SQLException
	{
		return parseItems(resultSet, 0);
	}

	@Override
	public void add(Versioned<FoodItem> item) throws DuplicateException, PersistenceException
	{
		save(Arrays.<Versioned<FoodItem>> asList(item));
	}

	@Override
	public int count(String prefix)
	{
		int userId = getCurrentUserId();

		if (prefix == null)
		{
			throw new NullPointerException("ID prefix can't be null");
		}

		try
		{
			final String[] select = { "COUNT(*)" };
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = null;

			ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);

			if (set.next())
			{
				int count = set.getInt(1);
				set.close();
				return count;
			}
			else
			{
				throw new IllegalStateException("Failed to request SQL database");
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public void delete(String id)
	{
		try
		{
			int userId = getCurrentUserId();

			Map<String, String> set = new HashMap<String, String>();
			set.put(COLUMN_FOODBASE_DELETED, Utils.formatBooleanInt(true));

			Map<String, String> where = new HashMap<String, String>();
			where.put(COLUMN_FOODBASE_GUID, id);
			where.put(COLUMN_FOODBASE_USER, String.valueOf(userId));

			db.update(TABLE_FOODBASE, set, where);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			String where;
			String[] whereArgs;

			if (includeRemoved)
			{
				where = String.format("(%s = ?)", COLUMN_FOODBASE_USER, userId);
				whereArgs = new String[] { String.valueOf(userId) };
			}
			else
			{
				where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_DELETED);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatBooleanInt(false) };
			}

			final String order = null;

			ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);

			List<Versioned<FoodItem>> result = parseItems(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?) AND (%s LIKE ?)", COLUMN_FOODBASE_USER,
					COLUMN_FOODBASE_DELETED, COLUMN_FOODBASE_NAMECACHE);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), "%" + filter + "%" };
			final String order = COLUMN_FOODBASE_NAMECACHE;

			ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);

			List<Versioned<FoodItem>> result = parseItems(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Versioned<FoodItem> findById(String id)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), id };
			final String order = null;

			ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);

			List<Versioned<FoodItem>> result = parseItems(set);
			set.close();
			return result.isEmpty() ? null : result.get(0);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<FoodItem>> findByIdPrefix(String prefix)
	{
		int userId = getCurrentUserId();

		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = COLUMN_FOODBASE_NAMECACHE;

			ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);

			List<Versioned<FoodItem>> result = parseItems(set, MAX_ITEMS_COUNT);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(Date since)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s >= ?)", COLUMN_FOODBASE_USER,
					COLUMN_FOODBASE_TIMESTAMP);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatTimeUTC(since) };
			final String order = COLUMN_FOODBASE_NAMECACHE;

			ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);

			List<Versioned<FoodItem>> result = parseItems(set);
			set.close();
			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?) AND (%s = ?)", COLUMN_FOODBASE_USER,
					COLUMN_FOODBASE_DELETED, COLUMN_FOODBASE_NAMECACHE);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), exactName };
			final String order = null;

			ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);

			List<Versioned<FoodItem>> result = parseItems(set);
			set.close();
			return result.isEmpty() ? null : result.get(0);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	/**
	 * Returns sorted map (ID, Hash) for all items
	 * 
	 * @return
	 */
	@SuppressWarnings("static-method")
	private SortedMap<String, String> getDataHashes(int userId)
	{
		try
		{
			final String[] select = { COLUMN_FOODBASE_GUID, COLUMN_FOODBASE_HASH };
			final String where = String.format("(%s = ?)", COLUMN_FOODBASE_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);

			SortedMap<String, String> result = new TreeMap<String, String>();

			while (set.next())
			{
				String id = set.getString(COLUMN_FOODBASE_GUID);
				String hash = set.getString(COLUMN_FOODBASE_HASH);
				// THINK: probably storing entries is unnecessary, so we should process it as we go
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

	@Override
	public String getHash(String prefix)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = { COLUMN_FOODBASE_HASH_HASH };
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOODBASE_HASH_USER,
					COLUMN_FOODBASE_HASH_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix };
			final String order = null;

			ResultSet set = db.select(TABLE_FOODBASE_HASH, select, where, whereArgs, order);

			String hash = null;

			if (set.next())
			{
				hash = set.getString(COLUMN_FOODBASE_HASH_HASH);
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
	public Map<String, String> getHashChildren(String prefix)
	{
		try
		{
			int userId = getCurrentUserId();

			Map<String, String> result = new HashMap<String, String>();

			if (prefix.length() < ObjectService.ID_PREFIX_SIZE)
			{
				final String[] select = { COLUMN_FOODBASE_HASH_GUID, COLUMN_FOODBASE_HASH_HASH };
				final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_FOODBASE_HASH_USER,
						COLUMN_FOODBASE_HASH_GUID);
				final String[] whereArgs = { String.valueOf(userId), prefix + "_" };
				final String order = null;

				ResultSet set = db.select(TABLE_FOODBASE_HASH, select, where, whereArgs, order);

				while (set.next())
				{
					String id = set.getString(COLUMN_FOODBASE_HASH_GUID);
					String hash = set.getString(COLUMN_FOODBASE_HASH_HASH);
					result.put(id, hash);
				}

				set.close();
			}
			else
			{
				final String[] select = { COLUMN_FOODBASE_GUID, COLUMN_FOODBASE_HASH };
				final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_FOODBASE_USER,
						COLUMN_FOODBASE_GUID);
				final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
				final String order = null;

				ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);

				while (set.next())
				{
					String id = set.getString(COLUMN_FOODBASE_GUID);
					String hash = set.getString(COLUMN_FOODBASE_HASH);
					result.put(id, hash);
				}

				set.close();
			}

			return result;
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public void save(List<Versioned<FoodItem>> items)
	{
		try
		{
			int userId = getCurrentUserId();

			for (Versioned<FoodItem> item : items)
			{
				final String content = serializer.write(item.getData());
				final String nameCache = item.getData().getName();
				final String timeStamp = Utils.formatTimeUTC(item.getTimeStamp());
				final String hash = item.getHash();
				final String version = String.valueOf(item.getVersion());
				final String deleted = Utils.formatBooleanInt(item.isDeleted());

				if (recordExists(userId, item.getId()))
				{
					// presented, update

					Map<String, String> set = new HashMap<String, String>();
					set.put(COLUMN_FOODBASE_TIMESTAMP, timeStamp);
					set.put(COLUMN_FOODBASE_HASH, hash);
					set.put(COLUMN_FOODBASE_VERSION, version);
					set.put(COLUMN_FOODBASE_DELETED, deleted);
					set.put(COLUMN_FOODBASE_CONTENT, content);
					set.put(COLUMN_FOODBASE_NAMECACHE, nameCache);

					Map<String, String> where = new HashMap<String, String>();
					where.put(COLUMN_FOODBASE_GUID, item.getId());
					where.put(COLUMN_FOODBASE_USER, String.valueOf(userId));

					db.update(TABLE_FOODBASE, set, where);
				}
				else
				{
					// not presented, insert

					Map<String, String> set = new HashMap<String, String>();
					set.put(COLUMN_FOODBASE_GUID, item.getId());
					set.put(COLUMN_FOODBASE_USER, String.valueOf(userId));
					set.put(COLUMN_FOODBASE_TIMESTAMP, timeStamp);
					set.put(COLUMN_FOODBASE_HASH, hash);
					set.put(COLUMN_FOODBASE_VERSION, version);
					set.put(COLUMN_FOODBASE_DELETED, deleted);
					set.put(COLUMN_FOODBASE_CONTENT, content);
					set.put(COLUMN_FOODBASE_NAMECACHE, nameCache);

					db.insert(TABLE_FOODBASE, set);
				}

				HashUtils.updateHashBranch(this, item.getId().substring(0, ID_PREFIX_SIZE));
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public MerkleTree getHashTree()
	{
		int userId = getCurrentUserId();

		SortedMap<String, String> hashes = getDataHashes(userId);
		SortedMap<String, String> tree = HashUtils.buildHashTree(hashes);

		MemoryMerkleTree result = new MemoryMerkleTree();
		result.putAll(tree); // headers (0..4 chars id)
		result.putAll(hashes); // leafs (32 chars id)
		return result;
	}

	@Override
	public void setHash(String prefix, String hash)
	{
		try
		{
			int userId = getCurrentUserId();

			if (prefix.length() <= ObjectService.ID_PREFIX_SIZE)
			{
				if (getHash(prefix) != null)
				{
					Map<String, String> set = new HashMap<String, String>();
					set.put(COLUMN_FOODBASE_HASH_HASH, hash);

					Map<String, String> where = new HashMap<String, String>();
					where.put(COLUMN_FOODBASE_HASH_USER, String.valueOf(userId));
					where.put(COLUMN_FOODBASE_HASH_GUID, prefix);

					db.update(TABLE_FOODBASE_HASH, set, where);
				}
				else
				{
					Map<String, String> set = new HashMap<String, String>();
					set.put(COLUMN_FOODBASE_HASH_USER, String.valueOf(userId));
					set.put(COLUMN_FOODBASE_HASH_GUID, prefix);
					set.put(COLUMN_FOODBASE_HASH_HASH, hash);

					db.insert(TABLE_FOODBASE_HASH, set);
				}
			}
			else if (prefix.length() == ObjectService.ID_FULL_SIZE)
			{
				if (recordExists(userId, prefix))
				{
					SortedMap<String, String> set = new TreeMap<String, String>();
					set.put(COLUMN_FOODBASE_HASH, hash);

					SortedMap<String, String> where = new TreeMap<String, String>();
					where.put(COLUMN_FOODBASE_USER, String.valueOf(userId));
					where.put(COLUMN_FOODBASE_GUID, prefix);

					db.update(TABLE_FOODBASE, set, where);
				}
				else
				{
					// fail
					throw new NotFoundException(prefix);
				}
			}
			else
			{
				throw new IllegalArgumentException(String.format(
						"Invalid prefix ('%s'), expected: 0..%d or %d chars, found: %d", prefix,
						ObjectService.ID_PREFIX_SIZE, ObjectService.ID_FULL_SIZE, prefix.length()));
			}
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@SuppressWarnings("static-method")
	private boolean recordExists(int userId, String id) throws SQLException
	{
		final String[] select = { COLUMN_FOODBASE_GUID };
		final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_FOODBASE_USER, COLUMN_FOODBASE_GUID);
		final String[] whereArgs = { String.valueOf(userId), id };
		final String order = null;

		ResultSet set = db.select(TABLE_FOODBASE, select, where, whereArgs, order);
		boolean result = set.first();

		set.close();
		return result;
	}
}
