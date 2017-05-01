/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.backend.features.dishbase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.android.backend.common.db.tables.TableDishbase;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.services.transfer.ImportService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;

public class DishBaseLocalService implements DishBaseService, ImportService
{
	private static final String						TAG				= DishBaseLocalService.class.getSimpleName();

	private static final int						MAX_READ_ITEMS	= 500;

	private final ContentResolver					resolver;
	private final Parser<DishItem>					parser			= new ParserDishItem();
	private final Serializer<DishItem>				serializer		= new SerializerAdapter<DishItem>(parser);
	private final Serializer<Versioned<DishItem>>	serializerV		= new SerializerAdapter<Versioned<DishItem>>(
			new ParserVersioned<DishItem>(parser));

	// caching
	// NOTE: this suppose DB can't be changed outside app
	public static List<Versioned<DishItem>>			memoryCache;

	// ====================================================================================

	public DishBaseLocalService(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new IllegalArgumentException("Content resolver is null");
		}
		this.resolver = resolver;

		if (memoryCache == null)
		{
			memoryCache = findInDB(null, null, true, null);
		}
	}

	/**
	 * Automatically closes cursor after read
	 * 
	 * @param cursor
	 * @param limit
	 * @return
	 */
	private List<Versioned<DishItem>> parseItems(Cursor cursor)
	{
		// analyze response
		if (cursor != null)
		{
			List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();

			int indexId = cursor.getColumnIndex(TableDishbase.COLUMN_ID);
			int indexTimeStamp = cursor.getColumnIndex(TableDishbase.COLUMN_TIMESTAMP);
			int indexHash = cursor.getColumnIndex(TableDishbase.COLUMN_HASH);
			int indexVersion = cursor.getColumnIndex(TableDishbase.COLUMN_VERSION);
			int indexData = cursor.getColumnIndex(TableDishbase.COLUMN_DATA);
			int indexDeleted = cursor.getColumnIndex(TableDishbase.COLUMN_DELETED);

			while (cursor.moveToNext())
			{
				String valueId = cursor.getString(indexId);
				Date valueTimeStamp = Utils.parseTimeUTC(cursor.getString(indexTimeStamp));
				String valueHash = cursor.getString(indexHash);
				int valueVersion = cursor.getInt(indexVersion);
				boolean valueDeleted = cursor.getInt(indexDeleted) == 1;
				String valueData = cursor.getString(indexData);

				long temp = System.currentTimeMillis();
				DishItem item = serializer.read(valueData);

				Versioned<DishItem> versioned = new Versioned<DishItem>(item);
				versioned.setId(valueId);
				versioned.setTimeStamp(valueTimeStamp);
				versioned.setHash(valueHash);
				versioned.setVersion(valueVersion);
				versioned.setDeleted(valueDeleted);

				result.add(versioned);
			}

			cursor.close();

			return result;
		}
		else
		{
			throw new IllegalArgumentException("Cursor is null");
		}
	}

	private static List<Versioned<DishItem>> find(String id, String name, boolean includeDeleted, Date modAfter)
	{
		return findInCache(id, name, includeDeleted, modAfter);
	}

	private List<Versioned<DishItem>> findInDB(String id, String name, boolean includeDeleted, Date modAfter)
	{
		long time = System.currentTimeMillis();

		try
		{
			// constructing parameters
			String[] columns = null; // all

			String where = "";
			List<String> whereArgs = new ArrayList<String>();

			if (id != null)
			{
				if (id.length() == ID_PREFIX_SIZE)
				{
					where += where.isEmpty() ? "" : " AND ";
					where += TableDishbase.COLUMN_ID + " LIKE ?";
					whereArgs.add(id + "%");
				}
				else
				{
					where += where.isEmpty() ? "" : " AND ";
					where += TableDishbase.COLUMN_ID + " = ?";
					whereArgs.add(id);
				}
			}

			if (name != null)
			{
				where += where.isEmpty() ? "" : " AND ";
				where += TableDishbase.COLUMN_NAMECACHE + " LIKE ?";
				whereArgs.add("%" + name + "%");
			}

			if (modAfter != null)
			{
				where += where.isEmpty() ? "" : " AND ";
				where += TableDishbase.COLUMN_TIMESTAMP + " > ?";
				whereArgs.add(Utils.formatTimeUTC(modAfter));
			}

			if (!includeDeleted)
			{
				where += where.isEmpty() ? "" : " AND ";
				where += TableDishbase.COLUMN_DELETED + " = 0";
			}

			String[] mSelectionArgs = whereArgs.toArray(new String[] {});
			String mSortOrder = TableDishbase.COLUMN_NAMECACHE;

			// execute query
			Cursor cursor = resolver.query(TableDishbase.CONTENT_URI, columns, where, mSelectionArgs, mSortOrder);

			final List<Versioned<DishItem>> result = parseItems(cursor);

			// Log.i(TAG, "Search done in " + (System.currentTimeMillis() - time) + " msec");
			return result;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	private static List<Versioned<DishItem>> findInCache(String id, String name, boolean includeDeleted, Date modAfter)
	{
		long time = System.currentTimeMillis();

		try
		{
			List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();
			if (name != null)
			{
				name = name.toLowerCase(Locale.US);
			}

			for (Versioned<DishItem> item : memoryCache)
			{
				if (((id == null) || item.getId().startsWith(id))
						&& ((name == null) || item.getData().getName().toLowerCase(Locale.US).contains(name))
						&& (includeDeleted || !item.isDeleted())
						&& ((modAfter == null) || item.getTimeStamp().after(modAfter)))
				{
					result.add(new Versioned<DishItem>(item));
				}
			}

			Collections.sort(result, new Comparator<Versioned<DishItem>>()
			{
				@Override
				public int compare(Versioned<DishItem> arg0, Versioned<DishItem> arg1)
				{
					return arg0.getData().getName().compareTo(arg1.getData().getName());
				}
			});

			// Log.i(TAG, "Search (cache) done in " + (System.currentTimeMillis() - time) +
			// " msec");
			return result;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	private boolean recordExists(String id)
	{
		final String[] select = { TableDishbase.COLUMN_ID };
		final String where = String.format("%s = ?", TableDishbase.COLUMN_ID);
		final String[] whereArgs = { id };
		final String sortOrder = null;

		Cursor cursor = resolver.query(TableDishbase.CONTENT_URI, select, where, whereArgs, sortOrder);

		try
		{
			return cursor != null && cursor.moveToFirst();
		}
		finally
		{
			if (cursor != null)
			{
				cursor.close();
			}
		}
	}

	private void insert(Versioned<DishItem> item)
	{
		ContentValues newValues = new ContentValues();
		newValues.put(TableDishbase.COLUMN_ID, item.getId());
		newValues.put(TableDishbase.COLUMN_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
		newValues.put(TableDishbase.COLUMN_HASH, item.getHash());
		newValues.put(TableDishbase.COLUMN_VERSION, item.getVersion());
		newValues.put(TableDishbase.COLUMN_DELETED, item.isDeleted());
		newValues.put(TableDishbase.COLUMN_NAMECACHE, item.getData().getName());
		newValues.put(TableDishbase.COLUMN_DATA, serializer.write(item.getData()));

		resolver.insert(TableDishbase.CONTENT_URI, newValues);
		updateCacheItem(item);
	}

	private void update(Versioned<DishItem> item)
	{
		ContentValues newValues = new ContentValues();
		newValues.put(TableDishbase.COLUMN_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
		newValues.put(TableDishbase.COLUMN_HASH, item.getHash());
		newValues.put(TableDishbase.COLUMN_VERSION, item.getVersion());
		newValues.put(TableDishbase.COLUMN_DELETED, item.isDeleted());
		newValues.put(TableDishbase.COLUMN_NAMECACHE, item.getData().getName());
		newValues.put(TableDishbase.COLUMN_DATA, serializer.write(item.getData()));

		String clause = TableDishbase.COLUMN_ID + " = ?";
		String[] args = { item.getId() };

		resolver.update(TableDishbase.CONTENT_URI, newValues, clause, args);
		updateCacheItem(item);
	}

	private static void updateCacheItem(Versioned<DishItem> item)
	{
		boolean found = false;

		for (Versioned<DishItem> x : memoryCache)
		{
			if (x.equals(item))
			{
				x.setTimeStamp(item.getTimeStamp());
				x.setHash(item.getHash());
				x.setVersion(item.getVersion());
				x.setDeleted(item.isDeleted());
				x.setData(item.getData()); // FIXME: may be problem
				found = true;
				break;
			}
		}

		if (!found)
		{
			memoryCache.add(new Versioned<DishItem>(item));
		}
	}

	// private List<SearchResult> loadHeadersFromDB(String name)
	// {
	// long time = System.currentTimeMillis();
	//
	// try
	// {
	// // constructing parameters
	// String[] mProj = { DiaryContentProvider.COLUMN_DISHBASE_GUID,
	// DiaryContentProvider.COLUMN_DISHBASE_NAMECACHE };
	//
	// String mSelectionClause = "";
	// List<String> args = new LinkedList<String>();
	//
	// if (name != null)
	// {
	// mSelectionClause += mSelectionClause.isEmpty() ? "" : " AND ";
	// mSelectionClause += DiaryContentProvider.COLUMN_DISHBASE_NAMECACHE + " LIKE ?";
	// args.add("%" + name + "%");
	// }
	//
	// mSelectionClause += mSelectionClause.isEmpty() ? "" : " AND ";
	// mSelectionClause += DiaryContentProvider.COLUMN_DISHBASE_DELETED + " = 0";
	//
	// String[] mSelectionArgs = args.toArray(new String[] {});
	// String mSortOrder = DiaryContentProvider.COLUMN_DISHBASE_NAMECACHE;
	//
	// // execute query
	// Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DISHBASE_URI, mProj,
	// mSelectionClause,
	// mSelectionArgs, mSortOrder);
	//
	// final List<SearchResult> result = parseHeaders(cursor);
	// cursor.close();
	//
	// Log.i(TAG, "Search headers (database) done in " + (System.currentTimeMillis() - time) +
	// " msec");
	// return result;
	// }
	// catch (Exception e)
	// {
	// throw new CommonServiceException(e);
	// }
	// }

	@Override
	public void add(Versioned<DishItem> item) throws DuplicateException
	{
		if (!recordExists(item.getId()))
		{
			insert(item);
		}
		else
		{
			throw new DuplicateException(item.getId());
		}
	}

	@Override
	public int count(String prefix)
	{
		if (prefix == null)
		{
			throw new IllegalArgumentException("ID prefix is null");
		}

		String[] projection = new String[] { "count(*) AS count" };
		String clause = String.format("%s LIKE ?", TableDishbase.COLUMN_ID);
		String[] clauseArgs = { prefix + "%" };

		Cursor cursor = resolver.query(TableDishbase.CONTENT_URI, projection, clause, clauseArgs, null);
		cursor.moveToFirst();
		int count = cursor.getInt(0);
		cursor.close();

		return count;
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		Versioned<DishItem> item = findById(id);

		if (item == null)
		{
			throw new NotFoundException(id);
		}
		if (item.isDeleted())
		{
			throw new AlreadyDeletedException(id);
		}

		item.setDeleted(true);
		item.modified();
		update(item);
	}

	@Override
	public List<Versioned<DishItem>> findAll(boolean includeRemoved)
	{
		return find(null, null, includeRemoved, null);
	}

	@Override
	public List<Versioned<DishItem>> findAny(String filter)
	{
		return find(null, filter, false, null);

		/*
		 * As far as SQLite LIKE operator is case-sensitive for non-latin chars, we need to filter
		 * it manually :(
		 */

		// List<Versioned<DishItem>> all = find(null, null, false, null);
		// List<Versioned<DishItem>> filtered = new LinkedList<Versioned<DishItem>>();
		// filter = filter.toLowerCase();
		//
		// for (Versioned<DishItem> item : all)
		// {
		// if (item.getData().getName().toLowerCase().contains(filter))
		// {
		// filtered.add(item);
		// }
		// }
		//
		// return filtered;
	}

	// @Override
	// public List<SearchResult> quickFindAny(String filter)
	// {
	// /*
	// * As far as SQLite LIKE operator is case-sensitive for non-latin chars, we need to filter
	// * it manually :(
	// */
	//
	// // List<SearchResult> items = loadHeadersFromDB(filter);
	// // return items;
	//
	// List<SearchResult> all = loadHeadersFromDB(""); // FIXME
	// List<SearchResult> filtered = new LinkedList<SearchResult>();
	// filter = filter.toLowerCase();
	//
	// for (SearchResult item : all)
	// {
	// if (item.getName().toLowerCase().contains(filter))
	// {
	// filtered.add(item);
	// }
	// }
	//
	// return filtered;
	// }

	@Override
	public Versioned<DishItem> findOne(String exactName)
	{
		exactName = exactName.trim();
		List<Versioned<DishItem>> all = find(null, exactName, false, null);

		for (Versioned<DishItem> item : all)
		{
			if (item.getData().getName().trim().equals(exactName))
			{
				return item;
			}
		}

		return null;
	}

	@Override
	public Versioned<DishItem> findById(String id)
	{
		List<Versioned<DishItem>> list = find(id, null, true, null);
		if (list.isEmpty())
		{
			return null;
		}
		else
		{
			return list.get(0);
		}
	}

	@Override
	public List<Versioned<DishItem>> findByIdPrefix(String prefix) throws CommonServiceException
	{
		List<Versioned<DishItem>> items = find(prefix, null, true, null);
		if (items.size() <= MAX_READ_ITEMS)
		{
			return items;
		}
		else
		{
			// workaround for satisfying specification
			throw new TooManyItemsException("Too many items");
		}
	}

	@Override
	public List<Versioned<DishItem>> findChanged(Date since)
	{
		return find(null, null, true, since);
	}

	/**
	 * Returns sorted map (ID, Hash) for all items
	 * 
	 * @return
	 */
	private static SortedMap<String, String> getDataHashes()
	{
		SortedMap<String, String> result = new TreeMap<String, String>();

		for (Versioned<DishItem> item : memoryCache)
		{
			result.put(item.getId(), item.getHash());
		}

		// // constructing parameters
		// final String[] select = { DiaryContentProvider.COLUMN_DISHBASE_GUID,
		// DiaryContentProvider.COLUMN_DISHBASE_HASH };
		// final String where = null;
		// final String[] whereArgs = null;
		//
		// // execute query
		// Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DISHBASE_URI, select, where,
		// whereArgs, null);
		//
		// // analyze response
		// int indexId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DISHBASE_GUID);
		// int indexHash = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DISHBASE_HASH);
		//
		// SortedMap<String, String> result = new TreeMap<String, String>();
		//
		// while (cursor.moveToNext())
		// {
		// String id = cursor.getString(indexId);
		// String hash = cursor.getString(indexHash);
		// // THINK: probably storing entries is unnecessary, so we should process it as we go
		// result.put(id, hash);
		// }
		//
		// cursor.close();
		return result;
	}

	@Override
	public MerkleTree getHashTree()
	{
		return HashUtils.buildMerkleTree(getDataHashes());
	}

	@Override
	public void save(List<Versioned<DishItem>> items)
	{
		for (Versioned<DishItem> item : items)
		{
			if (recordExists(item.getId()))
			{
				update(item);
			}
			else
			{
				insert(item);
			}
		}
	}

	@Override
	public void importData(String data)
	{
		// naive slow implementation
		List<Versioned<DishItem>> items = serializerV.readAll(data);
		save(items);
	}
}