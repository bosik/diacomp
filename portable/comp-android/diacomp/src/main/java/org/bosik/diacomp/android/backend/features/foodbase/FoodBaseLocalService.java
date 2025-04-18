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

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.util.Log;

import org.bosik.diacomp.android.backend.common.CachedBaseService;
import org.bosik.diacomp.android.backend.common.db.tables.TableFoodbase;
import org.bosik.diacomp.android.backend.features.quickImport.PlainDataImporter;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.transfer.Importable;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FoodBaseLocalService extends CachedBaseService<FoodItem> implements FoodBaseService, Importable
{
	private static final String TAG = FoodBaseLocalService.class.getSimpleName();

	private final Context              context;
	private final ContentResolver      resolver;
	private final Serializer<FoodItem> serializer = new SerializerAdapter<>(new ParserFoodItem());

	// =============================================================================================================================

	private static FoodBaseService instance;

	public static synchronized FoodBaseService getInstance(Context context)
	{
		if (instance == null)
		{
			Log.i(TAG, "Local food base initialization...");
			instance = new FoodBaseLocalService(context);
		}

		return instance;
	}

	// =============================================================================================================================

	private FoodBaseLocalService(Context context)
	{
		if (context == null)
		{
			throw new IllegalArgumentException("context is null");
		}

		this.context = context;
		this.resolver = context.getContentResolver();

		rebuildCache();
	}

	@Override
	protected List<Versioned<FoodItem>> loadAllFromDb()
	{
		try (Cursor cursor = resolver.query(TableFoodbase.CONTENT_URI, null, "", new String[] {}, null))
		{
			if (cursor == null)
			{
				throw new IllegalStateException("Cursor is null");
			}

			List<Versioned<FoodItem>> map = new ArrayList<>();

			int indexId = cursor.getColumnIndex(TableFoodbase.COLUMN_ID);
			int indexTimeStamp = cursor.getColumnIndex(TableFoodbase.COLUMN_TIMESTAMP);
			int indexHash = cursor.getColumnIndex(TableFoodbase.COLUMN_HASH);
			int indexVersion = cursor.getColumnIndex(TableFoodbase.COLUMN_VERSION);
			int indexData = cursor.getColumnIndex(TableFoodbase.COLUMN_DATA);
			int indexDeleted = cursor.getColumnIndex(TableFoodbase.COLUMN_DELETED);

			while (cursor.moveToNext())
			{
				FoodItem item = serializer.read(cursor.getString(indexData));

				Versioned<FoodItem> versioned = new Versioned<>(item);

				versioned.setId(cursor.getString(indexId));
				versioned.setTimeStamp(Utils.parseTimeUTC(cursor.getString(indexTimeStamp)));
				versioned.setHash(cursor.getString(indexHash));
				versioned.setVersion(cursor.getInt(indexVersion));
				versioned.setDeleted((cursor.getInt(indexDeleted) == 1));

				map.add(versioned);
			}

			return map;
		}
	}

	@Override
	protected void insertDb(Versioned<FoodItem> item)
	{
		ContentValues newValues = new ContentValues();
		newValues.put(TableFoodbase.COLUMN_ID, item.getId());
		newValues.put(TableFoodbase.COLUMN_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
		newValues.put(TableFoodbase.COLUMN_HASH, item.getHash());
		newValues.put(TableFoodbase.COLUMN_VERSION, item.getVersion());
		newValues.put(TableFoodbase.COLUMN_DELETED, item.isDeleted());
		newValues.put(TableFoodbase.COLUMN_DATA, serializer.write(item.getData()));
		newValues.put(TableFoodbase.COLUMN_NAMECACHE, item.getData().getName());

		resolver.insert(TableFoodbase.CONTENT_URI, newValues);
	}

	@Override
	protected void updateDb(Versioned<FoodItem> item)
	{
		ContentValues newValues = new ContentValues();
		newValues.put(TableFoodbase.COLUMN_TIMESTAMP, Utils.formatTimeUTC(item.getTimeStamp()));
		newValues.put(TableFoodbase.COLUMN_HASH, item.getHash());
		newValues.put(TableFoodbase.COLUMN_VERSION, item.getVersion());
		newValues.put(TableFoodbase.COLUMN_DELETED, item.isDeleted());
		newValues.put(TableFoodbase.COLUMN_DATA, serializer.write(item.getData()));
		newValues.put(TableFoodbase.COLUMN_NAMECACHE, item.getData().getName());

		String clause = TableFoodbase.COLUMN_ID + " = ?";
		String[] args = { item.getId() };

		resolver.update(TableFoodbase.CONTENT_URI, newValues, clause, args);
	}

	@Override
	public void importData(InputStream stream) throws IOException
	{
		new PlainDataImporter(context, TableFoodbase.INSTANCE, "1")
		{
			@Override
			protected void parseEntry(String[] items, ContentValues newValues)
			{
				if (items.length != 7)
				{
					throw new IllegalArgumentException("Invalid entry: " + Arrays.toString(items));
				}

				newValues.put(TableFoodbase.COLUMN_NAMECACHE, items[0]);
				newValues.put(TableFoodbase.COLUMN_ID, items[1]);
				newValues.put(TableFoodbase.COLUMN_TIMESTAMP, items[2]);
				newValues.put(TableFoodbase.COLUMN_HASH, items[3]);
				newValues.put(TableFoodbase.COLUMN_VERSION, Integer.parseInt(items[4]));
				newValues.put(TableFoodbase.COLUMN_DELETED, Boolean.parseBoolean(items[5]));
				newValues.put(TableFoodbase.COLUMN_DATA, items[6]);
			}
		}.importPlain(stream);

		rebuildCache();
	}
}