package org.bosik.diacomp.android.backend.features.search;

import java.util.HashMap;
import java.util.Map;
import org.bosik.diacomp.core.services.search.TagService;
import android.content.ContentResolver;

public class TagLocalService implements TagService
{
	private final ContentResolver		resolver;

	private static final Map<String, Integer>	cache	= new HashMap<String, Integer>();

	public TagLocalService(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new NullPointerException("Content resolver can't be null");
		}
		this.resolver = resolver;
	}

	/**
	 * Returns tag if found, null otherwise
	 * 
	 * @param id
	 * @return
	 */
	// private Integer readTag(String id)
	// {
	// try
	// {
	// // constructing parameters
	// final String[] select = { DiaryContentProvider.COLUMN_TAG_TAG };
	// final String where = DiaryContentProvider.COLUMN_TAG_GUID + " = ?";
	// final String[] whereArgs = new String[] { id };
	//
	// // execute query
	// Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_TAG_URI, select, where,
	// whereArgs, null);
	//
	// // analyze response
	// if (cursor != null)
	// {
	// int indexTag = 0;// cursor.getColumnIndex(DiaryContentProvider.COLUMN_TAG_TAG);
	// Integer valueTag = null;
	//
	// if (cursor.moveToNext())
	// {
	// valueTag = cursor.getInt(indexTag);
	// }
	//
	// cursor.close();
	// return valueTag;
	// }
	// else
	// {
	// throw new NullPointerException("Cursor is null");
	// }
	// }
	// catch (Exception e)
	// {
	// throw new CommonServiceException(e);
	// }
	// }

	// private void updateTag(String id, int tag)
	// {
	// try
	// {
	// final ContentValues newValues = new ContentValues();
	// newValues.put(DiaryContentProvider.COLUMN_TAG_TAG, tag);
	// final String where = DiaryContentProvider.COLUMN_TAG_GUID + " = ?";
	// String[] whereArgs = new String[] { id };
	// resolver.update(DiaryContentProvider.CONTENT_TAG_URI, newValues, where, whereArgs);
	// }
	// catch (PersistenceException e)
	// {
	// throw e;
	// }
	// catch (Exception e)
	// {
	// throw new PersistenceException(e);
	// }
	// }

	// private void insertTag(String id, int tag)
	// {
	// try
	// {
	// ContentValues newValues = new ContentValues();
	// newValues.put(DiaryContentProvider.COLUMN_TAG_GUID, id);
	// newValues.put(DiaryContentProvider.COLUMN_TAG_TAG, tag);
	// resolver.insert(DiaryContentProvider.CONTENT_TAG_URI, newValues);
	// }
	// catch (PersistenceException e)
	// {
	// throw e;
	// }
	// catch (Exception e)
	// {
	// throw new PersistenceException(e);
	// }
	// }

	@Override
	public Map<String, Integer> getTags()
	{
		return cache;
		// try
		// {
		// // constructing request (SELECT * FROM ...)
		// final String[] select = { DiaryContentProvider.COLUMN_TAG_GUID,
		// DiaryContentProvider.COLUMN_TAG_TAG };
		// final String where = null;// DiaryContentProvider.COLUMN_TAG_GUID + " = ?";
		// final String[] whereArgs = null;// new String[] { id };
		//
		// // execute query
		// Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_TAG_URI, select, where,
		// whereArgs, null);
		//
		// // analyze response
		// List<TagInfo> result = new ArrayList<TagInfo>();
		// if (cursor != null)
		// {
		// int columnId = cursor.getColumnIndex(DiaryContentProvider.COLUMN_TAG_GUID);
		// int columnTag = cursor.getColumnIndex(DiaryContentProvider.COLUMN_TAG_TAG);
		//
		// while (cursor.moveToNext())
		// {
		// String valueId = cursor.getString(columnId);
		// int valueTag = cursor.getInt(columnTag);
		// result.add(new TagInfo(valueId, valueTag));
		// }
		//
		// cursor.close();
		// return result;
		// }
		// else
		// {
		// throw new NullPointerException("Cursor is null");
		// }
		// }
		// catch (Exception e)
		// {
		// throw new CommonServiceException(e);
		// }
	}

	@Override
	public void incTag(String id, int value)
	{
		// Integer tag = readTag(id);
		// if (tag == null)
		// {
		// insertTag(id, value);
		// }
		// else
		// {
		// updateTag(id, tag + value);
		// }

		// for (TagInfo item : cache)
		// {
		// if (item.getId().equals(id))
		// {
		// item.setTag(item.getTag() + value);
		// return;
		// }
		// }
		//
		// cache.add(new TagInfo(id, value));

		Integer tag = cache.get(id);
		if (tag == null)
		{
			cache.put(id, value);
		}
		else
		{
			cache.put(id, tag + value);
		}
	}

	@Override
	public void reset()
	{
		// try
		// {
		// resolver.delete(DiaryContentProvider.CONTENT_TAG_URI, null, null);
		// }
		// catch (PersistenceException e)
		// {
		// throw e;
		// }
		// catch (Exception e)
		// {
		// throw new PersistenceException(e);
		// }
		cache.clear();
	}
}