package org.bosik.compensation.bo.diary.records;

public class BloodRecord extends DiaryRecord
{
	private static final long	serialVersionUID	= -1621097859834950338L;

	private double				value;
	private int					finger;

	// private boolean postPrand;

	public BloodRecord()
	{

	}

	public BloodRecord(int time, double value, int finger)
	{
		setTime(time);
		setValue(value);
		setFinger(finger);
	}

	// ================================ ВАЛИДАТОРЫ ================================

	public static boolean checkValue(double value)
	{
		return (value > 0); /* ограничение сверху отсутствует */
	}

	public static boolean checkFinger(int finger)
	{
		return (finger >= -1) && (finger <= 9);
	}

	public static boolean check(int time, double value, int finger)
	{
		return checkTime(time) && checkValue(value) && checkFinger(finger);
	}

	// ================================ GET / SET ================================

	public double getValue()
	{
		return value;
	}

	public void setValue(double value)
	{
		if (!checkValue(value))
		{
			throw new IllegalArgumentException("BloodRecord: неверное значение поля Value (" + value + ")");
		}

		if (value != this.value)
		{
			this.value = value;
			notifyModified();
		}
	}

	public int getFinger()
	{
		return finger;
	}

	public void setFinger(int value)
	{
		if (!checkFinger(value))
		{
			throw new IllegalArgumentException("BloodRecord: неверное значение поля Finger (" + value + ")");
		}

		if (value != finger)
		{
			finger = value;
			notifyModified();
		}
	}

}
