package org.bosik.diacomp.android.backend.common.db.tables;

import java.util.ArrayList;
import java.util.List;

import org.bosik.diacomp.android.backend.common.db.Column;
import org.bosik.diacomp.android.backend.common.db.Table;

import android.net.Uri;

public class TableDiary extends Table
{
	public static final String	COLUMN_ID			= "_GUID";
	public static final String	COLUMN_TIMESTAMP	= "_TimeStamp";
	public static final String	COLUMN_HASH			= "_Hash";
	public static final String	COLUMN_VERSION		= "_Version";
	public static final String	COLUMN_DELETED		= "_Deleted";
	public static final String	COLUMN_CONTENT		= "_Content";
	public static final String	COLUMN_TIMECACHE	= "_TimeCache";

	public static final Uri		CONTENT_URI			= new TableDiary().getUri();

	@Override
	public String getName()
	{
		return "diary";
	}

	@Override
	public String getContentType()
	{
		return "org.bosik.diacomp.diary";
	}

	@Override
	public List<Column> getColumns()
	{
		List<Column> columns = new ArrayList<>();

		columns.add(new Column(TableDiary.COLUMN_ID, Column.TYPE_TEXT, true, false));
		columns.add(new Column(TableDiary.COLUMN_TIMESTAMP, Column.TYPE_TEXT, false, false));
		columns.add(new Column(TableDiary.COLUMN_HASH, Column.TYPE_TEXT, false, false));
		columns.add(new Column(TableDiary.COLUMN_VERSION, Column.TYPE_INTEGER, false, false));
		columns.add(new Column(TableDiary.COLUMN_DELETED, Column.TYPE_INTEGER, false, false));
		columns.add(new Column(TableDiary.COLUMN_CONTENT, Column.TYPE_BLOB, false, false));
		columns.add(new Column(TableDiary.COLUMN_TIMECACHE, Column.TYPE_TEXT, false, false));

		return columns;
	}

}
