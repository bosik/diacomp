package org.bosik.compensation.persistence.dao.local;

import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.face.BuildConfig;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.dao.local.utils.DiaryContentProvider;
import org.bosik.compensation.persistence.exceptions.CommonDAOException;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.exceptions.StoreException;
import org.bosik.compensation.persistence.serializers.JSONConverter;
import org.bosik.compensation.persistence.serializers.JSONSerializer;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.persistence.serializers.foodbase.SerializerFoodItemJSON;
import org.bosik.compensation.utils.Utils;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;

public class NewLocalFoodBaseDAO implements FoodBaseDAO
{
	private ContentResolver			resolver;
	private Serializer<FoodItem>	serializer;

	public NewLocalFoodBaseDAO(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new NullPointerException("Content resolver can't be null");
		}
		this.resolver = resolver;

		JSONSerializer<FoodItem> s = new SerializerFoodItemJSON();
		serializer = new JSONConverter<FoodItem>(s);

	}

	@Override
	public String add(Versioned<FoodItem> item) throws StoreException
	{
		try
		{
			String code = serializer.write(item.getData());

			ContentValues newValues = new ContentValues();
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_GUID, item.getId());
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_VERSION, item.getVersion());
			newValues.put(DiaryContentProvider.COLUMN_FOODBASE_DATA, code);

			resolver.insert(DiaryContentProvider.CONTENT_FOODBASE_URI, newValues);

			return item.getId();
		}
		catch (Exception e)
		{
			throw new StoreException(e);
		}
	}

	@Override
	public void delete(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	@Override
	public List<Versioned<FoodItem>> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findById(String id)
	{
		try
		{
			if (null == id)
			{
				throw new NullPointerException("ID can't be null");
			}

			// constructing parameters
			String[] mProj = { DiaryContentProvider.COLUMN_FOODBASE_GUID,
					DiaryContentProvider.COLUMN_FOODBASE_TIMESTAMP, DiaryContentProvider.COLUMN_FOODBASE_VERSION,
					DiaryContentProvider.COLUMN_FOODBASE_DATA };
			String mSelectionClause = DiaryContentProvider.COLUMN_FOODBASE_GUID + " = ?";
			String[] mSelectionArgs = { id };
			String mSortOrder = null;

			// execute query
			Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_FOODBASE_URI, mProj, mSelectionClause,
					mSelectionArgs, mSortOrder);

			// analyze response
			if (cursor == null)
			{
				throw new NullPointerException("Cursor is null");
			}

			if (cursor.getCount() < 1)
			{
				return null;
			}

			if ((cursor.getCount() > 1) && (BuildConfig.DEBUG))
			{
				// на самом деле мы производим выборку по полю GUID, которое при создании
				// таблицы имеет атрибут UNIQUE, так что такого в принципе быть не должно

				throw new IllegalStateException("Several items found");
			}

			int indexTimeStamp = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_TIMESTAMP);
			int indexVersion = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_VERSION);
			int indexData = cursor.getColumnIndex(DiaryContentProvider.COLUMN_FOODBASE_DATA);
			cursor.moveToNext();

			Date timeStamp = Utils.parseTimeUTC(cursor.getString(indexTimeStamp));
			int version = cursor.getInt(indexVersion);
			String source = cursor.getString(indexData);

			FoodItem item = serializer.read(source);
			Versioned<FoodItem> result = new Versioned<FoodItem>(item);
			result.setId(id);
			result.setTimeStamp(timeStamp);
			result.setVersion(version);

			return result;
		}
		catch (Exception e)
		{
			throw new CommonDAOException(e);
		}
	}

	@Override
	public void update(Versioned<FoodItem> item) throws ItemNotFoundException, StoreException
	{
		// TODO Auto-generated method stub

	}
}
