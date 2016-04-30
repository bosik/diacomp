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

import org.bosik.diacomp.android.backend.common.DiaryContentProvider.MyDBHelper;
import org.bosik.diacomp.android.backend.common.db.Table;
import org.bosik.diacomp.android.backend.common.db.tables.TableKoofs;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;
import org.bosik.diacomp.core.utils.Utils;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;

public class KoofDao
{
	private ContentResolver	resolver;
	private Context			context;

	public KoofDao(Context context)
	{
		if (null == context)
		{
			throw new IllegalArgumentException("Context is null");
		}
		this.context = context;
		this.resolver = context.getContentResolver();
	}

	/* ======================= API ========================= */

	public void save(KoofList koofs)
	{
		SQLiteDatabase db = new MyDBHelper(context).getWritableDatabase();
		db.beginTransaction();
		try
		{
			Table table = new TableKoofs();
			for (int i = 0; i < Utils.MinPerDay; i++)
			{
				ContentValues values = new ContentValues();
				values.put(TableKoofs.COLUMN_VALUE_K, koofs.getKoof(i).getK());
				values.put(TableKoofs.COLUMN_VALUE_Q, koofs.getKoof(i).getQ());
				values.put(TableKoofs.COLUMN_VALUE_P, koofs.getKoof(i).getP());

				if (db.update(table.getName(), values, TableKoofs.COLUMN_TIME + " = ?", new String[] { String.valueOf(i) }) == 0)
				{
					values.put(TableKoofs.COLUMN_TIME, i);
					db.insert(table.getName(), null, values);
				}
			}
			db.setTransactionSuccessful();
		}
		finally
		{
			db.endTransaction();
			db.close();
		}
	}

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
			try
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
			finally
			{
				cursor.close();
			}
		}

		return null;
	}
}
