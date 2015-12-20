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
package org.bosik.diacomp.android.backend.features.foodbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MemoryMerkleTree;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;
import android.util.Log;

public class FoodBaseLocalService implements FoodBaseService
{
	private static final String				TAG				= FoodBaseLocalService.class.getSimpleName();

	private static final int				MAX_READ_ITEMS	= 500;

	private final ContentResolver			resolver;
	private final Serializer<FoodItem>		serializer;

	// caching
	// NOTE: this suppose DB can't be changed outside app
	public static List<Versioned<FoodItem>>	memoryCache;

	// ====================================================================================

	public FoodBaseLocalService(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new IllegalArgumentException("Content resolver is null");
		}
		this.resolver = resolver;

		Parser<FoodItem> s = new ParserFoodItem();
		serializer = new SerializerAdapter<FoodItem>(s);
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
	private List<Versioned<FoodItem>> parseItems(Cursor cursor)
	{
		// analyze response
		if (cursor != null)
		{
			long time = System.currentTimeMillis();

			List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();

			int indexId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_GUID);
			int indexTimeStamp = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_TIMESTAMP);
			int indexHash = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_HASH);
			int indexVersion = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_VERSION);
			int indexData = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_DATA);
			int indexDeleted = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_DELETED);

			long jsonTime = 0;

			while (cursor.moveToNext())
			{
				String valueId = cursor.getString(indexId);
				Date valueTimeStamp = Utils.parseTimeUTC(cursor.getString(indexTimeStamp));
				String valueHash = cursor.getString(indexHash);
				int valueVersion = cursor.getInt(indexVersion);
				boolean valueDeleted = cursor.getInt(indexDeleted) == 1;
				String valueData = cursor.getString(indexData);

				long temp = System.currentTimeMillis();
				FoodItem item = serializer.read(valueData);
				jsonTime += System.currentTimeMillis() - temp;

				Versioned<FoodItem> versioned = new Versioned<FoodItem>(item);
				versioned.setId(valueId);
				versioned.setTimeStamp(valueTimeStamp);
				versioned.setHash(valueHash);
				versioned.setVersion(valueVersion);
				versioned.setDeleted(valueDeleted);

				result.add(versioned);
			}

			Log.i(TAG, result.size() + " json's parsed in " + jsonTime + " msec");
			Log.i(TAG, result.size() + " items parsed in " + (System.currentTimeMillis() - time) + " msec");

			cursor.close();
			return result;
		}
		else
		{
			throw new IllegalArgumentException("Cursor is null");
		}
	}

	// private List<SearchResult> parseHeaders(Cursor cursor)
	// {
	// // analyze response
	// if (cursor != null)
	// {
	// List<SearchResult> result = new LinkedList<SearchResult>();
	//
	// int indexId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_GUID);
	// int indexName = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_NAMECACHE);
	//
	// while (cursor.moveToNext())
	// {
	// String valueId = cursor.getString(indexId);
	// String valueName = cursor.getString(indexName);
	// result.add(new SearchResult(valueId, valueName));
	// }
	//
	// return result;
	// }
	// else
	// {
	// throw new IllegalArgumentException("Cursor is null");
	// }
	// }

	private List<Versioned<FoodItem>> find(String id, String name, boolean includeDeleted, Date modAfter)
	{
		return findInCache(id, name, includeDeleted, modAfter);
	}

	private List<Versioned<FoodItem>> findInDB(String id, String name, boolean includeDeleted, Date modAfter)
	{
		long time = System.currentTimeMillis();

		try
		{
			// constructing parameters
			String[] columns = null; // all

			String where = "";
			List<String> whereArgs = new LinkedList<String>();

			if (id != null)
			{
				if (id.length() == ObjectService.ID_PREFIX_SIZE)
				{
					where += where.isEmpty() ? "" : " AND ";
					where += DiaryContentProvider.COLUMN_FOODBASE_GUID + " LIKE ?";
					whereArgs.add(id + "%");
				}
				else
				{
					where += where.isEmpty() ? "" : " AND ";
					where += DiaryContentProvider.COLUMN_FOODBASE_GUID + " = ?";
					whereArgs.add(id);
				}
			}

			if (name != null)
			{
				where += where.isEmpty() ? "" : " AND ";
				where += DiaryContentProvider.COLUMN_FOODBASE_NAMECACHE + " LIKE ?";
				whereArgs.add("%" + name + "%");
			}

			if (modAfter != null)
			{
				where += where.isEmpty() ? "" : " AND ";
				where += DiaryContentProvider.COLUMN_FOODBASE_TIMESTAMP + " > ?";
				whereArgs.add(Utils.formatTimeUTC(modAfter));
			}

			if (!includeDeleted)
			{
				where += where.isEmpty() ? "" : " AND ";
				where += DiaryContentProvider.COLUMN_FOODBASE_DELETED + " = 0";
			}

			String[] mSelectionArgs = whereArgs.toArray(new String[] {});
			String mSortOrder = DiaryContentProvider.COLUMN_FOODBASE_NAMECACHE;

			// execute query
			Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_FOODBASE_URI, columns, where, mSelectionArgs,
					mSortOrder);

			final List<Versioned<FoodItem>> result = parseItems(cursor);

			Log.i(TAG, "Search (database) done in " + (System.currentTimeMillis() - time) + " msec");
			return result;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	private List<Versioned<FoodItem>> findInCache(String id, String name, boolean includeDeleted, Date modAfter)
	{
		long time = System.currentTimeMillis();

		try
		{
			List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();
			if (name != null)
			{
				name = name.toLowerCase(Locale.US);
			}

			for (Versioned<FoodItem> item : memoryCache)
			{
				if (((id == null) || item.getId().startsWith(id))
						&& ((name == null) || item.getData().getName().toLowerCase(Locale.US).contains(name))
						&& (includeDeleted || !item.isDeleted())
						&& ((modAfter == null) || item.getTimeStamp().after(modAfter)))
				{
					result.add(new Versioned<FoodItem>(item));
				}
			}

			Collections.sort(result, new Comparator<Versioned<FoodItem>>()
			{
				@Override
				public int compare(Versioned<FoodItem> arg0, Versioned<FoodItem> arg1)
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
		final String[] select = { DiaryContentProvider.COLUMN_FOODBASE_GUID };
		final String where = String.format("%s = ?", DiaryContentProvider.COLUMN_FOODBASE_GUID);
		final String[] whereArgs = { id };
		final String sortOrder = null;

		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_FOODBASE_URI, select, where, whereArgs, sortOrder);

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

	// private List<SearchResult> loadHeadersFromDB(String name)
	// {
	// long time = System.currentTimeMillis();
	//
	// try
	// {
	// // constructing parameters
	// String[] mProj = { DiaryContentProvider.COLUMN_FOODBASE_GUID,
	// DiaryContentProvider.COLUMN_FOODBASE_NAMECACHE };
	//
	// String mSelectionClause = "";
	// List<String> args = new LinkedList<String>();
	//
	// if (name != null)
	// {
	// mSelectionClause += mSelectionClause.isEmpty() ? "" : " AND ";
	// mSelectionClause += DiaryContentProvider.COLUMN_FOODBASE_NAMECACHE + " LIKE ?";
	// args.add("%" + name + "%");
	// }
	//
	// mSelectionClause += mSelectionClause.isEmpty() ? "" : " AND ";
	// mSelectionClause += DiaryContentProvider.COLUMN_FOODBASE_DELETED + " = 0";
	//
	// String[] mSelectionArgs = args.toArray(new String[] {});
	// String mSortOrder = DiaryContentProvider.COLUMN_FOODBASE_NAMECACHE;
	//
	// // execute query
	// Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_FOODBASE_URI, mProj,
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
	public void add(Versioned<FoodItem> item) throws PersistenceException
	{
		try
		{
			ContentValues newValues = new ContentValues();
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_GUID, item.getId());
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_HASH, item.getHash());
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_VERSION, item.getVersion());
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_DELETED, item.isDeleted());
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_NAMECACHE, item.getData().getName());
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_DATA, serializer.write(item.getData()));

			resolver.insert(DiaryContentProvider.CONTENT_FOODBASE_URI, newValues);

			memoryCache.add(new Versioned<FoodItem>(item));
		}
		catch (PersistenceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new PersistenceException(e);
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
		String clause = String.format("%s LIKE ?", DiaryContentProvider.COLUMN_FOODBASE_GUID);
		String[] clauseArgs = { prefix + "%" };

		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_FOODBASE_URI, projection, clause, clauseArgs, null);
		cursor.moveToFirst();
		int count = cursor.getInt(0);
		cursor.close();

		return count;
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		Versioned<FoodItem> item = findById(id);

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
		save(Arrays.asList(item));
	}

	@Override
	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		return find(null, null, includeRemoved, null);
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		return find(null, filter, false, null);

		/*
		 * As far as SQLite LIKE operator is case-sensitive for non-latin chars, we need to filter
		 * it manually :(
		 */

		// List<Versioned<FoodItem>> all = find(null, null, false, null);
		// List<Versioned<FoodItem>> filtered = new LinkedList<Versioned<FoodItem>>();
		// filter = filter.toLowerCase();
		//
		// for (Versioned<FoodItem> item : all)
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
	// long time = System.currentTimeMillis();
	//
	// List<SearchResult> all = loadHeadersFromDB(null); // FIXME
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
	// time = System.currentTimeMillis() - time;
	// Log.d(TAG, String.format("quickFindAny: filtered by '%s' within %d msec", filter, time));
	//
	// return filtered;
	// }

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		exactName = exactName.trim();
		List<Versioned<FoodItem>> all = find(null, exactName, false, null);

		for (Versioned<FoodItem> item : all)
		{
			if (item.getData().getName().trim().equals(exactName))
			{
				return item;
			}
		}

		return null;
	}

	@Override
	public Versioned<FoodItem> findById(String id)
	{
		List<Versioned<FoodItem>> list = find(id, null, true, null);
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
	public List<Versioned<FoodItem>> findByIdPrefix(String prefix) throws CommonServiceException
	{
		List<Versioned<FoodItem>> items = find(prefix, null, true, null);
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
	public List<Versioned<FoodItem>> findChanged(Date since)
	{
		return find(null, null, true, since);
	}

	/**
	 * Returns sorted map (ID, Hash) for all items
	 * 
	 * @return
	 */
	private SortedMap<String, String> getDataHashes()
	{
		SortedMap<String, String> result = new TreeMap<String, String>();

		for (Versioned<FoodItem> item : memoryCache)
		{
			String id = item.getId();
			String hash = item.getHash();
			// THINK: probably storing entries is unnecessary, so we should process it as we go
			result.put(id, hash);
		}

		// =============================================================

		// // constructing parameters
		// final String[] select = { DiaryContentProvider.COLUMN_FOODBASE_GUID,
		// DiaryContentProvider.COLUMN_FOODBASE_HASH };
		// final String where = null;
		// final String[] whereArgs = null;
		//
		// // execute query
		// Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_FOODBASE_URI, select, where,
		// whereArgs, null);
		//
		// // analyze response
		// int indexId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_GUID);
		// int indexHash = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_HASH);
		//
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
		SortedMap<String, String> hashes = getDataHashes();
		SortedMap<String, String> tree = HashUtils.buildHashTree(hashes);

		MemoryMerkleTree result = new MemoryMerkleTree();
		result.putAll(tree); // headers (0..4 chars id)
		result.putAll(hashes); // leafs (32 chars id)
		return result;
	}

	@Override
	public void save(List<Versioned<FoodItem>> items) throws PersistenceException
	{
		try
		{
			for (Versioned<FoodItem> item : items)
			{
				ContentValues newValues = new ContentValues();
				newValues.put(DiaryContentProvider.COLUMN_FOODBASE_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
				newValues.put(DiaryContentProvider.COLUMN_FOODBASE_HASH, item.getHash());
				newValues.put(DiaryContentProvider.COLUMN_FOODBASE_VERSION, item.getVersion());
				newValues.put(DiaryContentProvider.COLUMN_FOODBASE_DELETED, item.isDeleted());
				String content = serializer.write(item.getData());
				newValues.put(DiaryContentProvider.COLUMN_FOODBASE_DATA, content);
				newValues.put(DiaryContentProvider.COLUMN_FOODBASE_NAMECACHE, item.getData().getName());

				// TODO: DB has new row, cache still doesn't
				// Thus app tries to insert row and ends up with PK constraint violation

				if (recordExists(item.getId()))
				{
					Log.v(TAG, "Updating item " + item.getId());
					String clause = DiaryContentProvider.COLUMN_FOODBASE_GUID + " = ?";
					String[] args = { item.getId() };
					resolver.update(DiaryContentProvider.CONTENT_FOODBASE_URI, newValues, clause, args);
				}
				else
				{
					Log.v(TAG, "Inserting item " + item.getId());
					newValues.put(DiaryContentProvider.COLUMN_FOODBASE_GUID, item.getId());
					resolver.insert(DiaryContentProvider.CONTENT_FOODBASE_URI, newValues);
				}
			}

			for (Versioned<FoodItem> item : items)
			{
				boolean found = false;

				for (Versioned<FoodItem> x : memoryCache)
				{
					if (x.getId().equals(item.getId()))
					{
						x.setTimeStamp(item.getTimeStamp());
						x.setVersion(item.getVersion());
						x.setDeleted(item.isDeleted());
						x.setData(item.getData()); // FIXME: may be problem
						found = true;
						break;
					}
				}

				if (!found)
				{
					memoryCache.add(new Versioned<FoodItem>(item));
				}
			}
		}
		catch (PersistenceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new PersistenceException(e);
		}
	}
}