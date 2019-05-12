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

import com.fasterxml.jackson.annotation.JsonProperty;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;

import java.util.Date;

public class InsRecord extends DiaryRecord
{
	private static final long	serialVersionUID	= 7357437083772571438L;

	public static final String TYPE = "ins";

	@JsonProperty("value")
	private double value;

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

	@Override
	public String getType()
	{
		return TYPE;
	}

	public double getValue()
	{
		return value;
	}

	public void setValue(double value)
	{
		this.value = value;
	}
}
