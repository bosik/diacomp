package org.bosik.compensation.bo.diary.records;

public class InsRecord extends DiaryRecord
{
	private double	value;

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

		if (value != this.value)
		{
			this.value = value;
			notifyModified();
		}
	}

}
