package org.bosik.diacomp.android.backend.common.db;

import android.database.Cursor;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.Function;

public class CursorIterator<T> implements Iterator<T>, Iterable<T>, AutoCloseable
{
	private final Cursor              cursor;
	private final Function<Cursor, T> mapper;

	public CursorIterator(Cursor cursor, Function<Cursor, T> mapper)
	{
		this.cursor = cursor;
		this.mapper = mapper;
	}

	@Override
	public boolean hasNext()
	{
		return !cursor.isClosed() && !cursor.isLast();
	}

	@Override
	public T next()
	{
		cursor.moveToNext();
		return mapper.apply(cursor);
	}

	@Override
	public void close()
	{
		cursor.close();
	}

	@Override
	public Iterator<T> iterator()
	{
		return this;
	}

	public static <T> CursorIterator<T> empty()
	{
		return new CursorIterator<T>(null, null)
		{
			@Override
			public boolean hasNext()
			{
				return false;
			}

			@Override
			public T next()
			{
				throw new NoSuchElementException();
			}

			@Override
			public void close()
			{
				// no-op
			}
		};
	}
}
