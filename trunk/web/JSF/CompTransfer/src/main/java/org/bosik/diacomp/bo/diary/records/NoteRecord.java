package org.bosik.diacomp.bo.diary.records;

import java.util.Date;
import org.bosik.diacomp.bo.diary.DiaryRecord;

public class NoteRecord extends DiaryRecord
{
	private static final long	serialVersionUID	= 7394847492375407284L;

	private String				text;

	public NoteRecord()
	{

	}

	public NoteRecord(Date time, String value)
	{
		setTime(time);
		setText(value);
	}

	// ================================ ВАЛИДАТОРЫ ================================

	public static boolean check(String value)
	{
		return (value != null);
	}

	// ================================ GET / SET ================================

	public String getText()
	{
		return text;
	}

	public void setText(String value)
	{
		if (!check(value))
		{
			throw new IllegalArgumentException("NoteRecord: неверное значение поля Text (" + value + ")");
		}

		text = value;
	}
}