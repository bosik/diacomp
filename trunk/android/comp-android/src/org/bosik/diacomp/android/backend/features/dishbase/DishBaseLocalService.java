package org.bosik.diacomp.android.backend.features.dishbase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.dishbase.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.utils.Utils;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;
import android.util.Log;

public class DishBaseLocalService implements DishBaseService
{
	private static final String				TAG	= DishBaseLocalService.class.getSimpleName();

	private final ContentResolver			resolver;
	private final Serializer<DishItem>		serializer;

	// caching
	// NOTE: this suppose DB can't be changed outside app
	public static List<Versioned<DishItem>>	memoryCache;

	// ====================================================================================

	public DishBaseLocalService(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new NullPointerException("Content resolver can't be null");
		}
		this.resolver = resolver;

		Parser<DishItem> s = new ParserDishItem();
		serializer = new SerializerAdapter<DishItem>(s);
		if (memoryCache == null)
		{
			memoryCache = findInDB(null, null, true, null);
		}
	}

	private List<Versioned<DishItem>> parseItems(Cursor cursor)
	{
		// analyze response
		if (cursor != null)
		{
			long time = System.currentTimeMillis();

			List<Versioned<DishItem>> result = new LinkedList<Versioned<DishItem>>();

			int indexId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DISHBASE_GUID);
			int indexTimeStamp = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DISHBASE_TIMESTAMP);
			int indexVersion = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DISHBASE_VERSION);
			int indexData = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DISHBASE_DATA);
			int indexDeleted = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DISHBASE_DELETED);

			long jsonTime = 0;

			while (cursor.moveToNext())
			{
				String valueId = cursor.getString(indexId);
				Date valueTimeStamp = Utils.parseTimeUTC(cursor.getString(indexTimeStamp));
				int valueVersion = cursor.getInt(indexVersion);
				boolean valueDeleted = cursor.getInt(indexDeleted) == 1;
				String valueData = cursor.getString(indexData);

				long temp = System.currentTimeMillis();
				DishItem item = serializer.read(valueData);
				jsonTime += System.currentTimeMillis() - temp;

				Versioned<DishItem> versioned = new Versioned<DishItem>(item);
				versioned.setId(valueId);
				versioned.setTimeStamp(valueTimeStamp);
				versioned.setVersion(valueVersion);
				versioned.setDeleted(valueDeleted);

				result.add(versioned);
			}

			// Log.i(TAG, result.size() + " json's parsed in " + jsonTime + " msec");
			// Log.i(TAG, result.size() + " items parsed in " + (System.currentTimeMillis() - time)
			// + " msec");

			return result;
		}
		else
		{
			throw new NullPointerException("Cursor is null");
		}
	}

	// private List<SearchResult> parseHeaders(Cursor cursor)
	// {
	// // analyze response
	// if (cursor != null)
	// {
	// List<SearchResult> result = new LinkedList<SearchResult>();
	//
	// int indexId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DISHBASE_GUID);
	// int indexName = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DISHBASE_NAMECACHE);
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
	// throw new NullPointerException("Cursor is null");
	// }
	// }

	private List<Versioned<DishItem>> find(String id, String name, boolean includeDeleted, Date modAfter)
	{
		return findInCache(id, name, includeDeleted, modAfter);
	}

	private List<Versioned<DishItem>> findInDB(String id, String name, boolean includeDeleted, Date modAfter)
	{
		long time = System.currentTimeMillis();

		try
		{
			// constructing parameters
			String[] mProj = { DiaryContentProvider.COLUMN_DISHBASE_GUID,
					DiaryContentProvider.COLUMN_DISHBASE_TIMESTAMP, DiaryContentProvider.COLUMN_DISHBASE_VERSION,
					DiaryContentProvider.COLUMN_DISHBASE_DELETED, DiaryContentProvider.COLUMN_DISHBASE_DATA };

			String mSelectionClause = "";
			List<String> args = new LinkedList<String>();

			if (id != null)
			{
				mSelectionClause += mSelectionClause.isEmpty() ? "" : " AND ";
				mSelectionClause += DiaryContentProvider.COLUMN_DISHBASE_GUID + " = ?";
				args.add(id);
			}

			if (name != null)
			{
				mSelectionClause += mSelectionClause.isEmpty() ? "" : " AND ";
				mSelectionClause += DiaryContentProvider.COLUMN_DISHBASE_NAMECACHE + " LIKE ?";
				args.add("%" + name + "%");
			}

			if (modAfter != null)
			{
				mSelectionClause += mSelectionClause.isEmpty() ? "" : " AND ";
				mSelectionClause += DiaryContentProvider.COLUMN_DISHBASE_TIMESTAMP + " > ?";
				args.add(Utils.formatTimeUTC(modAfter));
			}

			if (!includeDeleted)
			{
				mSelectionClause += mSelectionClause.isEmpty() ? "" : " AND ";
				mSelectionClause += DiaryContentProvider.COLUMN_DISHBASE_DELETED + " = 0";
			}

			String[] mSelectionArgs = args.toArray(new String[] {});
			String mSortOrder = DiaryContentProvider.COLUMN_DISHBASE_NAMECACHE;

			// execute query
			Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DISHBASE_URI, mProj, mSelectionClause,
					mSelectionArgs, mSortOrder);

			final List<Versioned<DishItem>> result = parseItems(cursor);
			cursor.close();

			// Log.i(TAG, "Search done in " + (System.currentTimeMillis() - time) + " msec");
			return result;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	private List<Versioned<DishItem>> findInCache(String id, String name, boolean includeDeleted, Date modAfter)
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
				if (((id == null) || item.getId().equals(id))
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
	public void add(Versioned<DishItem> item) throws PersistenceException
	{
		try
		{
			ContentValues newValues = new ContentValues();
			newValues.put(DiaryContentProvider.COLUMN_DISHBASE_GUID, item.getId());
			newValues.put(DiaryContentProvider.COLUMN_DISHBASE_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
			newValues.put(DiaryContentProvider.COLUMN_DISHBASE_VERSION, item.getVersion());
			newValues.put(DiaryContentProvider.COLUMN_DISHBASE_DELETED, item.isDeleted());
			newValues.put(DiaryContentProvider.COLUMN_DISHBASE_NAMECACHE, item.getData().getName());
			newValues.put(DiaryContentProvider.COLUMN_DISHBASE_DATA, serializer.write(item.getData()));

			resolver.insert(DiaryContentProvider.CONTENT_DISHBASE_URI, newValues);

			memoryCache.add(new Versioned<DishItem>(item));
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
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		try
		{
			Versioned<DishItem> founded = findById(id);

			if (founded == null)
			{
				throw new NotFoundException(id);
			}

			if (founded.isDeleted())
			{
				throw new AlreadyDeletedException(id);
			}

			ContentValues newValues = new ContentValues();
			newValues.put(DiaryContentProvider.COLUMN_DISHBASE_DELETED, 1);
			String[] args = new String[] { id };
			resolver.update(DiaryContentProvider.CONTENT_DISHBASE_URI, newValues,
					DiaryContentProvider.COLUMN_DISHBASE_GUID + " = ?", args);

			for (Versioned<DishItem> item : memoryCache)
			{
				if (item.getId().equals(id))
				{
					item.setDeleted(true);
					break;
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
		throw new UnsupportedOperationException();
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
	public List<Versioned<DishItem>> findChanged(Date since)
	{
		return find(null, null, true, since);
	}

	@Override
	public void save(List<Versioned<DishItem>> items) throws PersistenceException
	{
		try
		{
			for (Versioned<DishItem> item : items)
			{
				ContentValues newValues = new ContentValues();
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_VERSION, item.getVersion());
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_DELETED, item.isDeleted());
				String content = serializer.write(item.getData());
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_DATA, content);
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_NAMECACHE, item.getData().getName());

				if (findById(item.getId()) != null)
				{
					Log.v(TAG, "Updating item " + item.getId() + ": " + content);
					String clause = DiaryContentProvider.COLUMN_DISHBASE_GUID + " = ?";
					String[] args = new String[] { item.getId() };
					resolver.update(DiaryContentProvider.CONTENT_DISHBASE_URI, newValues, clause, args);
				}
				else
				{
					Log.v(TAG, "Inserting item " + item.getId() + ": " + content);
					newValues.put(DiaryContentProvider.COLUMN_DISHBASE_GUID, item.getId());
					resolver.insert(DiaryContentProvider.CONTENT_DISHBASE_URI, newValues);
				}
			}

			for (Versioned<DishItem> item : items)
			{
				boolean found = false;

				for (Versioned<DishItem> x : memoryCache)
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
					memoryCache.add(new Versioned<DishItem>(item));
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