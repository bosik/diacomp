package org.bosik.compensation.bo.diary.records;

import org.bosik.compensation.bo.diary.DiaryRecord;

public class InsRecord extends DiaryRecord
{
	private static final long	serialVersionUID	= 7357437083772571438L;

	private double				value;

	public InsRecord()
	{

	}

	public InsRecord(int time, double value)
	{
		setTime(time);
		setValue(value);
	}

	// ================================ ВАЛИДАТОРЫ ================================

	public static boolean check(double value)
	{
		return (value > 0);
	}

	public static boolean check(int time, double value)
	{
		return checkTime(time) && check(value);
	}

	// ================================ GET / SET ================================

	public double getValue()
	{
		return value;
	}

	public void setValue(double value)
	{
		if (!check(value))
		{
			throw new IllegalArgumentException("InsRecord: неверное значение поля Value (" + value + ")");
		}

		this.value = value;
	}
}
