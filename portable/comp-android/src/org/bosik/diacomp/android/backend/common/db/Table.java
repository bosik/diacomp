package org.bosik.diacomp.android.backend.common.db;

import java.util.List;

import org.bosik.diacomp.android.backend.common.DiaryContentProvider;

import android.net.Uri;

public abstract class Table
{
	public abstract String getName();

	@Deprecated
	public int getCode()
	{
		throw new UnsupportedOperationException();
	}
	
	public abstract String getContentType();

	public abstract List<Column> getColumns();
	
	public Uri getUri()
	{
		return DiaryContentProvider.buildUri(this);
	}
}
