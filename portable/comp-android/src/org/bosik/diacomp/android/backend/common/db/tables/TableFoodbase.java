package org.bosik.diacomp.android.backend.common.db.tables;

import java.util.ArrayList;
import java.util.List;

import org.bosik.diacomp.android.backend.common.db.Column;
import org.bosik.diacomp.android.backend.common.db.Table;

import android.net.Uri;

public class TableFoodbase extends Table
{
	public static final String	COLUMN_ID			= "GUID";
	public static final String	COLUMN_TIMESTAMP	= "TimeStamp";
	public static final String	COLUMN_HASH			= "Hash";
	public static final String	COLUMN_VERSION		= "Version";
	public static final String	COLUMN_DELETED		= "Deleted";
	public static final String	COLUMN_DATA			= "Data";
	public static final String	COLUMN_NAMECACHE	= "NameCache";

	public static final Uri		CONTENT_URI			= new TableFoodbase().getUri();
	public static final int		CODE				= 2;

	@Override
	public String getName()
	{
		return "foodbase";
	}
	
	@Override
	public int getCode()
	{
		return CODE;
	}

	@Override
	public String getContentType()
	{
		return "org.bosik.diacomp.food";
	}

	@Override
	public List<Column> getColumns()
	{
		List<Column> columns = new ArrayList<>();

		columns.add(new Column(COLUMN_ID, Column.TYPE_TEXT, true, false));
		columns.add(new Column(COLUMN_TIMESTAMP, Column.TYPE_TEXT, false, false));
		columns.add(new Column(COLUMN_HASH, Column.TYPE_TEXT, false, false));
		columns.add(new Column(COLUMN_VERSION, Column.TYPE_INTEGER, false, false));
		columns.add(new Column(COLUMN_DELETED, Column.TYPE_INTEGER, false, false));
		columns.add(new Column(COLUMN_NAMECACHE, Column.TYPE_TEXT, false, false));
		columns.add(new Column(COLUMN_DATA, Column.TYPE_TEXT, false, false));

		return columns;
	}
}
