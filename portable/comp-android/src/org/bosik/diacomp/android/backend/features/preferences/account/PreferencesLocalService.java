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
package org.bosik.diacomp.android.backend.features.preferences.account;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.android.backend.common.db.tables.TablePreferences;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserPreferenceEntry;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.preferences.Preference;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferencesService;
import org.bosik.diacomp.core.services.transfer.Importable;
import org.bosik.diacomp.core.utils.Utils;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.util.Log;

public class PreferencesLocalService extends PreferencesService implements Importable
{
	static final String									TAG			= PreferencesLocalService.class.getSimpleName();

	private final Context								context;
	private final ContentResolver						resolver;

	private final Parser<PreferenceEntry<String>>		parser		= new ParserPreferenceEntry();
	private final Serializer<PreferenceEntry<String>>	serializer	= new SerializerAdapter<PreferenceEntry<String>>(
			parser);

	public PreferencesLocalService(Context context)
	{
		if (context == null)
		{
			throw new IllegalArgumentException("Context is null");
		}

		this.context = context;
		this.resolver = context.getContentResolver();
	}

	@Override
	public List<PreferenceEntry<String>> getAll()
	{
		// construct parameters
		String[] projection = null; // all
		String clause = null;
		String[] clauseArgs = {};
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(TablePreferences.CONTENT_URI, projection, clause, clauseArgs, sortOrder);

		try
		{
			if (cursor != null)
			{
				List<PreferenceEntry<String>> result = new ArrayList<PreferenceEntry<String>>();

				while (cursor.moveToNext())
				{
					int indexKey = cursor.getColumnIndex(TablePreferences.COLUMN_KEY);
					int indexValue = cursor.getColumnIndex(TablePreferences.COLUMN_VALUE);
					int indexVersion = cursor.getColumnIndex(TablePreferences.COLUMN_VERSION);

					try
					{
						PreferenceEntry<String> entry = new PreferenceEntry<String>();

						entry.setType(Preference.parse(cursor.getString(indexKey)));
						entry.setValue(cursor.getString(indexValue));
						entry.setVersion(cursor.getInt(indexVersion));

						result.add(entry);
					}
					catch (IllegalArgumentException e)
					{
						Log.w("Failed to parse preference", e);
					}
				}

				return result;
			}
			else
			{
				throw new IllegalArgumentException("Cursor is null");
			}
		}
		finally
		{
			if (cursor != null)
			{
				cursor.close();
			}
		}
	}

	@Override
	public void update(List<PreferenceEntry<String>> entries)
	{
		for (PreferenceEntry<String> entry : entries)
		{
			setString(entry);
		}
	}

	@Override
	public PreferenceEntry<String> getString(Preference preference)
	{
		// construct parameters
		String[] projection = null; // all
		String clause = TablePreferences.COLUMN_KEY + " = ?";
		String[] clauseArgs = { preference.getKey() };
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(TablePreferences.CONTENT_URI, projection, clause, clauseArgs, sortOrder);

		try
		{
			if (cursor != null)
			{
				if (cursor.moveToFirst())
				{
					int indexKey = cursor.getColumnIndex(TablePreferences.COLUMN_KEY);
					int indexValue = cursor.getColumnIndex(TablePreferences.COLUMN_VALUE);
					int indexVersion = cursor.getColumnIndex(TablePreferences.COLUMN_VERSION);

					PreferenceEntry<String> entry = new PreferenceEntry<String>();
					entry.setType(Preference.parse(cursor.getString(indexKey)));
					entry.setValue(cursor.getString(indexValue));
					entry.setVersion(cursor.getInt(indexVersion));

					return entry;
				}
				else
				{
					return null;
				}
			}
			else
			{
				throw new IllegalArgumentException("Cursor is null");
			}
		}
		finally
		{
			if (cursor != null)
			{
				cursor.close();
			}
		}
	}

	@Override
	public void setString(PreferenceEntry<String> entry)
	{
		try
		{
			String key = entry.getType().getKey();
			boolean exists = entryExists(key);

			ContentValues newValues = new ContentValues();

			newValues.put(TablePreferences.COLUMN_VALUE, entry.getValue());
			newValues.put(TablePreferences.COLUMN_VERSION, entry.getVersion());

			if (exists)
			{
				String clause = TablePreferences.COLUMN_KEY + " = ?";
				String[] args = { key };
				resolver.update(TablePreferences.CONTENT_URI, newValues, clause, args);
			}
			else
			{
				newValues.put(TablePreferences.COLUMN_KEY, key);
				resolver.insert(TablePreferences.CONTENT_URI, newValues);
			}
		}
		catch (Exception e)
		{
			throw new PersistenceException(e);
		}
	}

	private boolean entryExists(String key)
	{
		final String[] select = { TablePreferences.COLUMN_VERSION };
		final String where = String.format("%s = ?", TablePreferences.COLUMN_KEY);
		final String[] whereArgs = { key };
		final String sortOrder = null;

		Cursor cursor = resolver.query(TablePreferences.CONTENT_URI, select, where, whereArgs, sortOrder);

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

	@Override
	public void importData(InputStream stream) throws IOException
	{
		String s = Utils.readStream(stream);
		List<PreferenceEntry<String>> items = serializer.readAll(s);
		update(items);

		// Table table = new TablePreferences();
		//
		// SQLiteDatabase db = new MyDBHelper(context).getWritableDatabase();
		// db.beginTransaction();
		// try
		// {
		// ContentValues newValues = new ContentValues();
		//
		// for (PreferenceEntry<String> item : items)
		// {
		// newValues.put(TablePreferences.COLUMN_KEY, item.getType().getKey());
		// newValues.put(TablePreferences.COLUMN_VALUE, item.getValue());
		// newValues.put(TablePreferences.COLUMN_VERSION, item.getVersion());
		//
		// db.insertWithOnConflict(table.getName(), null, newValues,
		// SQLiteDatabase.CONFLICT_IGNORE);
		// }
		//
		// db.setTransactionSuccessful();
		// }
		// finally
		// {
		// db.endTransaction();
		// db.close();
		// resolver.notifyChange(TablePreferences.CONTENT_URI, null);
		// }
	}
}
