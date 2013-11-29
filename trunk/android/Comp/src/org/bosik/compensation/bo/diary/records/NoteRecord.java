package org.bosik.compensation.bo.diary.records;

public class NoteRecord extends DiaryRecord
{
	private static final long	serialVersionUID	= 7394847492375407284L;

	// TODO: remove this initialization
	private String				text				= "";

	public NoteRecord(int time, String value)
	{
		setTime(time);
		setText(value);
	}

	// ================================ ВАЛИДАТОРЫ ================================

	public static boolean check(String value)
	{
		return (value != null);
	}

	public static boolean check(int time, String value)
	{
		return checkTime(time) && check(value);
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