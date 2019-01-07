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

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider.MyDBHelper;
import org.bosik.diacomp.android.backend.common.db.Table;
import org.bosik.diacomp.android.backend.common.db.tables.TableRates;
import org.bosik.diacomp.core.services.analyze.entities.Rate;
import org.bosik.diacomp.core.services.analyze.entities.RateList;
import org.bosik.diacomp.core.utils.Utils;

public class RatesDao
{
	private ContentResolver	resolver;
	private Context			context;

	public RatesDao(Context context)
	{
		if (null == context)
		{
			throw new IllegalArgumentException("Context is null");
		}
		this.context = context;
		this.resolver = context.getContentResolver();
	}

	/* ======================= API ========================= */

	public void save(RateList rateList)
	{
		SQLiteDatabase db = new MyDBHelper(context).getWritableDatabase();
		db.beginTransaction();
		try
		{
			Table table = new TableRates();
			for (int i = 0; i < Utils.MinPerDay; i++)
			{
				ContentValues values = new ContentValues();
				values.put(TableRates.COLUMN_VALUE_K, rateList.getRate(i).getK());
				values.put(TableRates.COLUMN_VALUE_Q, rateList.getRate(i).getQ());
				values.put(TableRates.COLUMN_VALUE_P, rateList.getRate(i).getP());

				if (db.update(table.getName(), values, TableRates.COLUMN_TIME + " = ?", new String[] { String.valueOf(i) }) == 0)
				{
					values.put(TableRates.COLUMN_TIME, i);
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

	public Rate find(int time)
	{
		// construct parameters
		String[] projection = null; // all
		String clause = TableRates.COLUMN_TIME + " = ?";
		String[] clauseArgs = { String.valueOf(time) };
		String sortOrder = null;

		// execute
		Cursor cursor = resolver.query(TableRates.CONTENT_URI, projection, clause, clauseArgs, sortOrder);

		if (cursor != null)
		{
			try
			{
				int indexK = cursor.getColumnIndexOrThrow(TableRates.COLUMN_VALUE_K);
				int indexQ = cursor.getColumnIndexOrThrow(TableRates.COLUMN_VALUE_Q);
				int indexP = cursor.getColumnIndexOrThrow(TableRates.COLUMN_VALUE_P);

				if (cursor.moveToNext())
				{
					double k = cursor.getDouble(indexK);
					double q = cursor.getDouble(indexQ);
					double p = cursor.getDouble(indexP);

					return new Rate(k, q, p);
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
