package org.bosik.diacomp.android.backend.common.db.tables;

import java.util.ArrayList;
import java.util.List;

import org.bosik.diacomp.android.backend.common.db.Column;
import org.bosik.diacomp.android.backend.common.db.Table;

import android.net.Uri;

public class TableTags extends Table
{
	public static final String	COLUMN_ID	= "GUID";
	public static final String	COLUMN_TAG	= "Tag";

	public static final Uri		CONTENT_URI	= new TableTags().getUri();

	@Override
	public String getName()
	{
		return "tag";
	}

	@Override
	public String getContentType()
	{
		return "org.bosik.diacomp.tag";
	}

	@Override
	public List<Column> getColumns()
	{
		List<Column> columns = new ArrayList<>();

		columns.add(new Column(COLUMN_ID, Column.TYPE_TEXT, true, false));
		columns.add(new Column(COLUMN_TAG, Column.TYPE_INTEGER, false, false));

		return columns;
	}
}
