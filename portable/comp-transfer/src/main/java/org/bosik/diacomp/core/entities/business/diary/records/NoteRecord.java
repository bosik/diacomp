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

public class NoteRecord extends DiaryRecord
{
	private static final long	serialVersionUID	= 7394847492375407284L;

	public static final String TYPE = "note";

	@JsonProperty("text")
	private String text;

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

	@Override
	public String getType()
	{
		return TYPE;
	}

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
