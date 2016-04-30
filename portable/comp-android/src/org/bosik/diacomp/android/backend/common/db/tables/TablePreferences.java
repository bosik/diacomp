package org.bosik.diacomp.android.backend.common.db.tables;

import java.util.ArrayList;
import java.util.List;

import org.bosik.diacomp.android.backend.common.db.Column;
import org.bosik.diacomp.android.backend.common.db.Table;

import android.net.Uri;

public class TablePreferences extends Table
{
	public static final String	COLUMN_KEY		= "Key";
	public static final String	COLUMN_VALUE	= "Value";
	public static final String	COLUMN_VERSION	= "Version";

	public static final Uri		CONTENT_URI		= new TablePreferences().getUri();

	@Override
	public String getName()
	{
		return "preferences";
	}

	@Override
	public String getContentType()
	{
		return "org.bosik.diacomp.preferences";
	}

	@Override
	public List<Column> getColumns()
	{
		List<Column> columns = new ArrayList<>();

		columns.add(new Column(COLUMN_KEY, Column.TYPE_TEXT, true, false));
		columns.add(new Column(COLUMN_VALUE, Column.TYPE_TEXT, false, false));
		columns.add(new Column(COLUMN_VERSION, Column.TYPE_INTEGER, false, false));

		return columns;
	}
}
