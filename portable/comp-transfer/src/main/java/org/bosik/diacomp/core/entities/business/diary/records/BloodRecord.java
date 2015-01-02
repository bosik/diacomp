package org.bosik.diacomp.core.entities.business.diary.records;

import java.util.Date;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;

public class BloodRecord extends DiaryRecord
{
	private static final long	serialVersionUID	= -1621097859834950338L;

	private double				value;
	private int					finger;
	private transient boolean	postPrand;

	public BloodRecord()
	{

	}

	/**
	 * Constructor
	 * 
	 * @param time
	 *            Time of blood sugar level measurement
	 * @param value
	 *            Blood sugar level. Valid values: positive
	 * @param finger
	 *            Number of finger. Valid values are [0...9]
	 */
	public BloodRecord(Date time, double value, int finger)
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

		this.value = value;
	}

	public int getFinger()
	{
		return finger;
	}

	/**
	 * Number of finger. Valid values are [0...9]
	 * 
	 * @param value
	 */
	public void setFinger(int value)
	{
		if (!checkFinger(value))
		{
			throw new IllegalArgumentException("BloodRecord: неверное значение поля Finger (" + value + ")");
		}

		finger = value;
	}

	public boolean isPostPrand()
	{
		return postPrand;
	}

	public void setPostPrand(boolean postPrand)
	{
		this.postPrand = postPrand;
	}
}
