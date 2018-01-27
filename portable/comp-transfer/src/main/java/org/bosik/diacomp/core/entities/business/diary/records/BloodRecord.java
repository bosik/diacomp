/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.entities.business.diary.records;

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;

import java.util.Date;
import java.util.Locale;

public class BloodRecord extends DiaryRecord
{
	private static final long serialVersionUID = -1621097859834950338L;

	private double  value;
	private int     finger;
	private boolean postPrand;

	public BloodRecord()
	{

	}

	/**
	 * Constructor
	 *
	 * @param time   Time of blood sugar level measurement
	 * @param value  Blood sugar level. Valid values: positive
	 * @param finger Number of finger. Valid values are [0...9]
	 */
	public BloodRecord(Date time, double value, int finger)
	{
		setTime(time);
		setValue(value);
		setFinger(finger);
	}

	// ================================ VALIDATORS ================================

	public static boolean checkValue(double value)
	{
		return (value > 0); /* no max limit */
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
		//		if (!checkValue(value))
		//		{
		//			throw new IllegalArgumentException("BloodRecord: неверное значение поля Value (" + value + ")");
		//		}

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
		//		if (!checkFinger(value))
		//		{
		//			throw new IllegalArgumentException("BloodRecord: неверное значение поля Finger (" + value + ")");
		//		}

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

	@Override
	public String toString()
	{
		return String.format(Locale.US, "Time: %s, Value: %.1f, Finger: %d", getTime(), getValue(), getFinger());
	}
}
