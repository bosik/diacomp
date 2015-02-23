package org.bosik.diacomp.web.backend.features.base.dish;

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
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.services.sync.HashUtils;
import org.bosik.diacomp.core.services.sync.MemoryMerkleTree;
import org.bosik.diacomp.core.services.sync.MerkleTree;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.MySQLAccess;
import org.bosik.diacomp.web.backend.common.MySQLAccess.DataCallback;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class DishBaseLocalService implements DishBaseService
{
	// Dishbase table
	private static final String					TABLE_DISHBASE				= "dishbase2";
	private static final String					COLUMN_DISHBASE_GUID		= "_GUID";
	private static final String					COLUMN_DISHBASE_USER		= "_UserID";
	private static final String					COLUMN_DISHBASE_TIMESTAMP	= "_TimeStamp";
	private static final String					COLUMN_DISHBASE_HASH		= "_Hash";
	private static final String					COLUMN_DISHBASE_VERSION		= "_Version";
	private static final String					COLUMN_DISHBASE_DELETED		= "_Deleted";
	private static final String					COLUMN_DISHBASE_CONTENT		= "_Content";
	private static final String					COLUMN_DISHBASE_NAMECACHE	= "_NameCache";

	// Dishbase hash table
	private static final String					TABLE_DISHBASE_HASH			= "dishbase_hash";
	private static final String					COLUMN_DISHBASE_HASH_USER	= "_UserID";
	private static final String					COLUMN_DISHBASE_HASH_GUID	= "_GUID";
	private static final String					COLUMN_DISHBASE_HASH_HASH	= "_Hash";

	private static final Parser<DishItem>		parser						= new ParserDishItem();
	private static final Serializer<DishItem>	serializer					= new SerializerAdapter<DishItem>(parser);

	@Autowired
	private UserInfoService						userInfoService;

	protected int getCurrentUserId()
	{
		return userInfoService.getCurrentUserId();
	}

	static List<Versioned<DishItem>> parseItems(ResultSet resultSet, int limit) throws SQLException
	{
		if (limit > 0)
		{
			if (resultSet.last())
			{
				if (resultSet.getRow() > limit)
				{
					throw new TooManyItemsException("Too many items");
				}

				resultSet.beforeFirst();
			}
		}

		List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();

		while (resultSet.next())
		{
			String id = resultSet.getString(COLUMN_DISHBASE_GUID);
			Date timeStamp = Utils.parseTimeUTC(resultSet.getString(COLUMN_DISHBASE_TIMESTAMP));
			String hash = resultSet.getString(COLUMN_DISHBASE_HASH);
			int version = resultSet.getInt(COLUMN_DISHBASE_VERSION);
			boolean deleted = (resultSet.getInt(COLUMN_DISHBASE_DELETED) == 1);
			String content = resultSet.getString(COLUMN_DISHBASE_CONTENT);

			Versioned<DishItem> item = new Versioned<DishItem>();
			item.setId(id);
			item.setTimeStamp(timeStamp);
			item.setHash(hash);
			item.setVersion(version);
			item.setDeleted(deleted);
			item.setData(serializer.read(content));

			result.add(item);
		}

		return result;
	}

	static List<Versioned<DishItem>> parseItems(ResultSet resultSet) throws SQLException
	{
		return parseItems(resultSet, 0);
	}

	@Override
	public void add(Versioned<DishItem> item) throws DuplicateException, PersistenceException
	{
		save(Arrays.<Versioned<DishItem>> asList(item));
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
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<Integer>()
			{
				@Override
				public Integer onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						return set.getInt(1);
					}
					else
					{
						throw new IllegalStateException("Failed to request SQL database");
					}
				}
			});
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
			set.put(COLUMN_DISHBASE_DELETED, Utils.formatBooleanInt(true));

			Map<String, String> where = new HashMap<String, String>();
			where.put(COLUMN_DISHBASE_GUID, id);
			where.put(COLUMN_DISHBASE_USER, String.valueOf(userId));

			MySQLAccess.update(TABLE_DISHBASE, set, where);
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<DishItem>> findAll(boolean includeRemoved)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			String where;
			String[] whereArgs;

			if (includeRemoved)
			{
				where = String.format("(%s = ?)", COLUMN_DISHBASE_USER, userId);
				whereArgs = new String[] { String.valueOf(userId) };
			}
			else
			{
				where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_DELETED);
				whereArgs = new String[] { String.valueOf(userId), Utils.formatBooleanInt(false) };
			}

			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order,
					new DataCallback<List<Versioned<DishItem>>>()
					{
						@Override
						public List<Versioned<DishItem>> onData(ResultSet set) throws SQLException
						{
							return parseItems(set);
						}
					});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<DishItem>> findAny(String filter)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?) AND (%s LIKE ?)", COLUMN_DISHBASE_USER,
					COLUMN_DISHBASE_DELETED, COLUMN_DISHBASE_NAMECACHE);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), "%" + filter + "%" };
			final String order = COLUMN_DISHBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order,
					new DataCallback<List<Versioned<DishItem>>>()
					{
						@Override
						public List<Versioned<DishItem>> onData(ResultSet set) throws SQLException
						{
							return parseItems(set);
						}
					});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Versioned<DishItem> findById(String id)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), id };
			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order,
					new DataCallback<Versioned<DishItem>>()
					{
						@Override
						public Versioned<DishItem> onData(ResultSet set) throws SQLException
						{
							List<Versioned<DishItem>> result = parseItems(set);
							return result.isEmpty() ? null : result.get(0);
						}
					});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<DishItem>> findByIdPrefix(String prefix)
	{
		int userId = getCurrentUserId();

		try
		{
			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix + "%" };
			final String order = COLUMN_DISHBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order,
					new DataCallback<List<Versioned<DishItem>>>()
					{
						@Override
						public List<Versioned<DishItem>> onData(ResultSet set) throws SQLException
						{
							return parseItems(set, MAX_ITEMS_COUNT);
						}
					});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Versioned<DishItem>> findChanged(Date since)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s >= ?)", COLUMN_DISHBASE_USER,
					COLUMN_DISHBASE_TIMESTAMP);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatTimeUTC(since) };
			final String order = COLUMN_DISHBASE_NAMECACHE;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order,
					new DataCallback<List<Versioned<DishItem>>>()
					{
						@Override
						public List<Versioned<DishItem>> onData(ResultSet set) throws SQLException
						{
							return parseItems(set, MAX_ITEMS_COUNT);
						}
					});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Versioned<DishItem> findOne(String exactName)
	{
		try
		{
			int userId = getCurrentUserId();

			final String[] select = null; // all
			final String where = String.format("(%s = ?) AND (%s = ?) AND (%s = ?)", COLUMN_DISHBASE_USER,
					COLUMN_DISHBASE_DELETED, COLUMN_DISHBASE_NAMECACHE);
			final String[] whereArgs = { String.valueOf(userId), Utils.formatBooleanInt(false), exactName };
			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order,
					new DataCallback<Versioned<DishItem>>()
					{
						@Override
						public Versioned<DishItem> onData(ResultSet set) throws SQLException
						{
							List<Versioned<DishItem>> result = parseItems(set);
							return result.isEmpty() ? null : result.get(0);
						}
					});
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
			final String[] select = { COLUMN_DISHBASE_GUID, COLUMN_DISHBASE_HASH };
			final String where = String.format("(%s = ?)", COLUMN_DISHBASE_USER);
			final String[] whereArgs = { String.valueOf(userId) };
			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order,
					new DataCallback<SortedMap<String, String>>()
					{
						@Override
						public SortedMap<String, String> onData(ResultSet set) throws SQLException
						{
							SortedMap<String, String> result = new TreeMap<String, String>();

							while (set.next())
							{
								String id = set.getString(COLUMN_DISHBASE_GUID);
								String hash = set.getString(COLUMN_DISHBASE_HASH);
								// THINK: probably storing entries is unnecessary, so we should process it as we go
								result.put(id, hash);
							}

							return result;
						}
					});
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

			final String[] select = { COLUMN_DISHBASE_HASH_HASH };
			final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DISHBASE_HASH_USER,
					COLUMN_DISHBASE_HASH_GUID);
			final String[] whereArgs = { String.valueOf(userId), prefix };
			final String order = null;

			return MySQLAccess.select(TABLE_DISHBASE_HASH, select, where, whereArgs, order, new DataCallback<String>()
			{
				@Override
				public String onData(ResultSet set) throws SQLException
				{
					if (set.next())
					{
						return set.getString(COLUMN_DISHBASE_HASH_HASH);
					}
					else
					{
						return null;
					}
				}
			});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public Map<String, String> getHashChildren(String prefix)
	{
		int userId = getCurrentUserId();

		try
		{
			String[] select;
			String where;
			String[] whereArgs;
			String order = null;

			if (prefix.length() < ObjectService.ID_PREFIX_SIZE)
			{
				select = new String[] { COLUMN_DISHBASE_HASH_GUID, COLUMN_DISHBASE_HASH_HASH };
				where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DISHBASE_HASH_USER, COLUMN_DISHBASE_HASH_GUID);
				whereArgs = new String[] { String.valueOf(userId), prefix + "_" };
			}
			else
			{
				select = new String[] { COLUMN_DISHBASE_GUID, COLUMN_DISHBASE_HASH };
				where = String.format("(%s = ?) AND (%s LIKE ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_GUID);
				whereArgs = new String[] { String.valueOf(userId), prefix + "%" };
			}

			return MySQLAccess.select(TABLE_DISHBASE_HASH, select, where, whereArgs, order,
					new DataCallback<Map<String, String>>()
					{
						@Override
						public Map<String, String> onData(ResultSet set) throws SQLException
						{
							Map<String, String> result = new HashMap<String, String>();

							while (set.next())
							{
								String id = set.getString(COLUMN_DISHBASE_GUID);
								String hash = set.getString(COLUMN_DISHBASE_HASH);
								result.put(id, hash);
							}

							return result;
						}
					});
		}
		catch (SQLException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public void save(List<Versioned<DishItem>> items)
	{
		try
		{
			int userId = getCurrentUserId();

			for (Versioned<DishItem> item : items)
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
					set.put(COLUMN_DISHBASE_TIMESTAMP, timeStamp);
					set.put(COLUMN_DISHBASE_HASH, hash);
					set.put(COLUMN_DISHBASE_VERSION, version);
					set.put(COLUMN_DISHBASE_DELETED, deleted);
					set.put(COLUMN_DISHBASE_CONTENT, content);
					set.put(COLUMN_DISHBASE_NAMECACHE, nameCache);

					Map<String, String> where = new HashMap<String, String>();
					where.put(COLUMN_DISHBASE_GUID, item.getId());
					where.put(COLUMN_DISHBASE_USER, String.valueOf(userId));

					MySQLAccess.update(TABLE_DISHBASE, set, where);
				}
				else
				{
					// not presented, insert

					Map<String, String> set = new HashMap<String, String>();
					set.put(COLUMN_DISHBASE_GUID, item.getId());
					set.put(COLUMN_DISHBASE_USER, String.valueOf(userId));
					set.put(COLUMN_DISHBASE_TIMESTAMP, timeStamp);
					set.put(COLUMN_DISHBASE_HASH, hash);
					set.put(COLUMN_DISHBASE_VERSION, version);
					set.put(COLUMN_DISHBASE_DELETED, deleted);
					set.put(COLUMN_DISHBASE_CONTENT, content);
					set.put(COLUMN_DISHBASE_NAMECACHE, nameCache);

					MySQLAccess.insert(TABLE_DISHBASE, set);
				}
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
					set.put(COLUMN_DISHBASE_HASH_HASH, hash);

					Map<String, String> where = new HashMap<String, String>();
					where.put(COLUMN_DISHBASE_HASH_USER, String.valueOf(userId));
					where.put(COLUMN_DISHBASE_HASH_GUID, prefix);

					MySQLAccess.update(TABLE_DISHBASE_HASH, set, where);
				}
				else
				{
					Map<String, String> set = new HashMap<String, String>();
					set.put(COLUMN_DISHBASE_HASH_USER, String.valueOf(userId));
					set.put(COLUMN_DISHBASE_HASH_GUID, prefix);
					set.put(COLUMN_DISHBASE_HASH_HASH, hash);

					MySQLAccess.insert(TABLE_DISHBASE_HASH, set);
				}
			}
			else if (prefix.length() == ObjectService.ID_FULL_SIZE)
			{
				if (recordExists(userId, prefix))
				{
					SortedMap<String, String> set = new TreeMap<String, String>();
					set.put(COLUMN_DISHBASE_HASH, hash);

					SortedMap<String, String> where = new TreeMap<String, String>();
					where.put(COLUMN_DISHBASE_USER, String.valueOf(userId));
					where.put(COLUMN_DISHBASE_GUID, prefix);

					MySQLAccess.update(TABLE_DISHBASE, set, where);
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
		final String[] select = { COLUMN_DISHBASE_GUID };
		final String where = String.format("(%s = ?) AND (%s = ?)", COLUMN_DISHBASE_USER, COLUMN_DISHBASE_GUID);
		final String[] whereArgs = { String.valueOf(userId), id };
		final String order = null;

		return MySQLAccess.select(TABLE_DISHBASE, select, where, whereArgs, order, new DataCallback<Boolean>()
		{
			@Override
			public Boolean onData(ResultSet set) throws SQLException
			{
				return set.first();
			}
		});
	}
}
