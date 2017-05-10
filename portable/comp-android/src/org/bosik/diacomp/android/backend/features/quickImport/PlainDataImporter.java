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
package org.bosik.diacomp.android.backend.features.quickImport;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider.MyDBHelper;
import org.bosik.diacomp.android.backend.common.db.Table;
import android.content.ContentValues;
import android.content.Context;
import android.database.sqlite.SQLiteDatabase;

public abstract class PlainDataImporter
{
	private Context	context;
	private Table	table;
	private String	version;

	public PlainDataImporter(Context context, Table table, String version)
	{
		this.context = context;
		this.table = table;
		this.version = version;
	}

	public void importPlain(InputStream stream) throws IOException
	{
		BufferedReader r = new BufferedReader(new InputStreamReader(stream));
		String line = r.readLine();

		// -------------------------------- header validation ----------------------------------

		if (line == null)
		{
			throw new IllegalArgumentException("Empty input");
		}

		String[] versionData = line.split("=");
		if (versionData.length != 2 || !"VERSION".equals(versionData[0]))
		{
			throw new IllegalArgumentException("Incorrect version header: " + line);
		}

		if (!version.equals(versionData[1]))
		{
			throw new IllegalArgumentException("Unsupported version: " + versionData[1]);
		}

		// -------------------------------- data processing ----------------------------------

		SQLiteDatabase db = new MyDBHelper(context).getWritableDatabase();
		db.beginTransaction();

		try
		{
			ContentValues newValues = new ContentValues();
			int count = 0;

			while ((line = r.readLine()) != null)
			{
				parseEntry(line.split("\t"), newValues);
				db.insertWithOnConflict(table.getName(), null, newValues, SQLiteDatabase.CONFLICT_IGNORE);

				if (++count % 1000 == 0)
				{
					db.setTransactionSuccessful();
					db.endTransaction();
					db.beginTransaction();
				}
			}

			db.setTransactionSuccessful();
		}
		finally
		{
			db.endTransaction();
			db.close();
			context.getContentResolver().notifyChange(table.getUri(), null);
		}
	}

	abstract protected void parseEntry(String[] items, ContentValues newValues);
}