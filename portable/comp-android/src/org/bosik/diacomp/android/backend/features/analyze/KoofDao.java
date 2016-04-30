/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.android.backend.features.analyze;

import org.bosik.diacomp.android.backend.common.db.tables.TableKoofs;
import org.bosik.diacomp.core.services.analyze.entities.Koof;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;

public class KoofDao
{
	private ContentResolver resolver;

	public KoofDao(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new IllegalArgumentException("Content Resolver is null");
		}
		this.resolver = resolver;
	}

	/* ======================= ROUTINES ========================= */

	private void insert(int time, Koof record)
	{
		ContentValues newValues = new ContentValues();

		newValues.put(TableKoofs.COLUMN_TIME, time);
		newValues.put(TableKoofs.COLUMN_VALUE_K, record.getK());
		newValues.put(TableKoofs.COLUMN_VALUE_Q, record.getQ());
		newValues.put(TableKoofs.COLUMN_VALUE_P, record.getP());

		resolver.insert(TableKoofs.CONTENT_URI, newValues);
	}

	private void update(int time, Koof record)
	{
		ContentValues newValues = new ContentValues();

		newValues.put(TableKoofs.COLUMN_VALUE_K, record.getK());
		newValues.put(TableKoofs.COLUMN_VALUE_Q, record.getQ());
		newValues.put(TableKoofs.COLUMN_VALUE_P, record.getP());

		String clause = TableKoofs.COLUMN_TIME + " = ?";
		String[] args = { String.valueOf(time) };

		resolver.update(TableKoofs.CONTENT_URI, newValues, clause, args);
	}

	private boolean exists(int time)
	{
		// construct parameters
		String[] projection = null; // all
		String clause = TableKoofs.COLUMN_TIME + " = ?";
		String[] clauseArgs = { String.valueOf(time) };
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(TableKoofs.CONTENT_URI, projection, clause, clauseArgs, sortOrder);
		return cursor != null && cursor.moveToNext();
	}

	/* ======================= API ========================= */

	public Koof find(int time)
	{
		// construct parameters
		String[] projection = null; // all
		String clause = TableKoofs.COLUMN_TIME + " = ?";
		String[] clauseArgs = { String.valueOf(time) };
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(TableKoofs.CONTENT_URI, projection, clause, clauseArgs, sortOrder);

		if (cursor != null)
		{
			int indexK = cursor.getColumnIndexOrThrow(TableKoofs.COLUMN_VALUE_K);
			int indexQ = cursor.getColumnIndexOrThrow(TableKoofs.COLUMN_VALUE_Q);
			int indexP = cursor.getColumnIndexOrThrow(TableKoofs.COLUMN_VALUE_P);

			if (cursor.moveToNext())
			{
				double k = cursor.getDouble(indexK);
				double q = cursor.getDouble(indexQ);
				double p = cursor.getDouble(indexP);

				return new Koof(k, q, p);
			}
		}

		return null;
	}

	public void save(int time, Koof record)
	{
		// TODO: optimize with bulk insert
		if (exists(time))
		{
			update(time, record);
		}
		else
		{
			insert(time, record);
		}
	}
}
