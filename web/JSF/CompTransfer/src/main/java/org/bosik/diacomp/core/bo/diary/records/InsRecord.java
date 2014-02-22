package org.bosik.diacomp.core.bo.diary.records;

import java.util.Date;
import org.bosik.diacomp.core.bo.diary.DiaryRecord;

public class InsRecord extends DiaryRecord
{
	private static final long	serialVersionUID	= 7357437083772571438L;

	private double				value;

	public InsRecord()
	{

	}

	public InsRecord(Date time, double value)
	{
		setTime(time);
		setValue(value);
	}

	// ================================ ВАЛИДАТОРЫ ================================

	public static boolean check(double value)
	{
		return (value > 0);
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
