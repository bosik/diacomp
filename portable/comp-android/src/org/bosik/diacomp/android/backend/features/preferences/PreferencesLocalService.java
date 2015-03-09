package org.bosik.diacomp.android.backend.features.preferences;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.preferences.Preference;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;
import android.util.Log;

public class PreferencesLocalService extends PreferencesTypedService
{
	static final String				TAG	= PreferencesLocalService.class.getSimpleName();

	private final ContentResolver	resolver;

	public PreferencesLocalService(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new IllegalArgumentException("Content resolver is null");
		}
		this.resolver = resolver;
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
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_PREFERENCES_URI, projection, clause, clauseArgs,
				sortOrder);

		try
		{
			if (cursor != null)
			{
				List<PreferenceEntry<String>> result = new ArrayList<PreferenceEntry<String>>();

				while (cursor.moveToNext())
				{
					int indexKey = cursor.getColumnIndex(DiaryContentProvider.COLUMN_PREFERENCES_KEY);
					int indexValue = cursor.getColumnIndex(DiaryContentProvider.COLUMN_PREFERENCES_VALUE);
					int indexVersion = cursor.getColumnIndex(DiaryContentProvider.COLUMN_PREFERENCES_VERSION);

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
		String clause = DiaryContentProvider.COLUMN_PREFERENCES_KEY + " = ?";
		String[] clauseArgs = { preference.getKey() };
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_PREFERENCES_URI, projection, clause, clauseArgs,
				sortOrder);

		try
		{
			if (cursor != null)
			{
				if (cursor.moveToFirst())
				{
					int indexKey = cursor.getColumnIndex(DiaryContentProvider.COLUMN_PREFERENCES_KEY);
					int indexValue = cursor.getColumnIndex(DiaryContentProvider.COLUMN_PREFERENCES_VALUE);
					int indexVersion = cursor.getColumnIndex(DiaryContentProvider.COLUMN_PREFERENCES_VERSION);

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

			newValues.put(DiaryContentProvider.COLUMN_PREFERENCES_VALUE, entry.getValue());
			newValues.put(DiaryContentProvider.COLUMN_PREFERENCES_VERSION, entry.getVersion());

			if (exists)
			{
				Log.v(TAG, "Updating item " + key + ": " + entry.getValue());

				String clause = DiaryContentProvider.COLUMN_PREFERENCES_KEY + " = ?";
				String[] args = { key };
				resolver.update(DiaryContentProvider.CONTENT_PREFERENCES_URI, newValues, clause, args);
			}
			else
			{
				Log.v(TAG, "Inserting item " + key + ": " + entry.getValue());

				newValues.put(DiaryContentProvider.COLUMN_PREFERENCES_KEY, key);
				resolver.insert(DiaryContentProvider.CONTENT_PREFERENCES_URI, newValues);
			}
		}
		catch (Exception e)
		{
			throw new PersistenceException(e);
		}
	}

	private boolean entryExists(String key)
	{
		final String[] select = { DiaryContentProvider.COLUMN_PREFERENCES_VERSION };
		final String where = String.format("%s = ?", DiaryContentProvider.COLUMN_PREFERENCES_KEY);
		final String[] whereArgs = { key };
		final String sortOrder = null;

		Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_PREFERENCES_URI, select, where, whereArgs,
				sortOrder);

		try
		{
			return cursor != null && cursor.moveToFirst();
		}
		finally
		{
			cursor.close();
		}
	}
}
