package org.bosik.diacomp.android.backend.common.db.tables;

import java.util.ArrayList;
import java.util.List;

import org.bosik.diacomp.android.backend.common.db.Column;
import org.bosik.diacomp.android.backend.common.db.Table;

import android.net.Uri;

public class TableKoofs extends Table
{
	public static final String	COLUMN_TIME		= "Time";
	public static final String	COLUMN_VALUE_K	= "K";
	public static final String	COLUMN_VALUE_Q	= "Q";
	public static final String	COLUMN_VALUE_P	= "P";

	public static final Uri		CONTENT_URI		= new TableKoofs().getUri();

	@Override
	public String getName()
	{
		return "koofs";
	}

	@Override
	public String getContentType()
	{
		return "org.bosik.diacomp.koofs";
	}

	@Override
	public List<Column> getColumns()
	{
		List<Column> columns = new ArrayList<>();

		columns.add(new Column(COLUMN_TIME, Column.TYPE_INTEGER, true, false));
		columns.add(new Column(COLUMN_VALUE_K, Column.TYPE_REAL, false, false));
		columns.add(new Column(COLUMN_VALUE_Q, Column.TYPE_REAL, false, false));
		columns.add(new Column(COLUMN_VALUE_P, Column.TYPE_REAL, false, false));

		return columns;
	}
}
