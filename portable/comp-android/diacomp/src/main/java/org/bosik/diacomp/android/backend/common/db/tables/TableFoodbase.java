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
package org.bosik.diacomp.android.backend.common.db.tables;

import android.net.Uri;

import org.bosik.diacomp.android.backend.common.db.Column;
import org.bosik.diacomp.android.backend.common.db.Table;

import java.util.ArrayList;
import java.util.List;

public final class TableFoodbase extends Table
{
	public static final Table INSTANCE    = new TableFoodbase();
	public static final Uri   CONTENT_URI = INSTANCE.getUri();

	public static final String TABLE_NAME       = "foodbase";
	public static final String COLUMN_ID        = "GUID";
	public static final String COLUMN_TIMESTAMP = "TimeStamp";
	public static final String COLUMN_HASH      = "Hash";
	public static final String COLUMN_VERSION   = "Version";
	public static final String COLUMN_DELETED   = "Deleted";
	public static final String COLUMN_DATA      = "Data";
	public static final String COLUMN_NAMECACHE = "NameCache";

	@Override
	public String getName()
	{
		return TABLE_NAME;
	}

	@Override
	public List<Column> getColumns()
	{
		List<Column> columns = new ArrayList<>();

		columns.add(new Column(COLUMN_ID, Column.Type.TEXT, true, false));
		columns.add(new Column(COLUMN_TIMESTAMP, Column.Type.TEXT, false, false));
		columns.add(new Column(COLUMN_HASH, Column.Type.TEXT, false, false));
		columns.add(new Column(COLUMN_VERSION, Column.Type.INTEGER, false, false));
		columns.add(new Column(COLUMN_DELETED, Column.Type.INTEGER, false, false));
		columns.add(new Column(COLUMN_NAMECACHE, Column.Type.TEXT, false, false));
		columns.add(new Column(COLUMN_DATA, Column.Type.TEXT, false, false));

		return columns;
	}

	private TableFoodbase()
	{
	}
}
