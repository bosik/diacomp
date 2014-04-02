package org.bosik.diacomp.android.backend.features.dishbase;

import java.util.Date;
import java.util.LinkedList;
import java.util.List;
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
	private static final String			TAG	= DishBaseLocalService.class.getSimpleName();

	private final ContentResolver		resolver;
	private final Serializer<DishItem>	serializer;

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

			Log.i(TAG, result.size() + " json's parsed in " + jsonTime + " msec");
			Log.i(TAG, result.size() + " items parsed in " + (System.currentTimeMillis() - time) + " msec");

			return result;
		}
		else
		{
			throw new NullPointerException("Cursor is null");
		}
	}

	private List<Versioned<DishItem>> find(String id, String name, boolean includeDeleted, Date modAfter)
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

			Log.i(TAG, "Search done in " + (System.currentTimeMillis() - time) + " msec");
			return result;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

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
			resolver.update(DiaryContentProvider.CONTENT_DISHBASE_URI, newValues, "GUID = ?", args);
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
		// return find(null, filter, false, null);

		/*
		 * As far as SQLite LIKE operator is case-sensitive for non-latin chars, we need to filter
		 * it manually :(
		 */

		List<Versioned<DishItem>> all = find(null, null, false, null);
		List<Versioned<DishItem>> filtered = new LinkedList<Versioned<DishItem>>();
		filter = filter.toLowerCase();

		for (Versioned<DishItem> item : all)
		{
			if (item.getData().getName().toLowerCase().contains(filter))
			{
				filtered.add(item);
			}
		}

		return filtered;
	}

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
				// Versioned<DishItem> founded = findById(item.getId());
				// if ((founded == null) || founded.isDeleted())
				// {
				// throw new NotFoundException(item.getId());
				// }

				ContentValues newValues = new ContentValues();
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_VERSION, item.getVersion());
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_DELETED, item.isDeleted());
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_DATA, serializer.write(item.getData()));
				newValues.put(DiaryContentProvider.COLUMN_DISHBASE_NAMECACHE, item.getData().getName());

				String[] args = new String[] { item.getId() };
				resolver.update(DiaryContentProvider.CONTENT_DISHBASE_URI, newValues, "GUID = ?", args);
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