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
package org.bosik.diacomp.core.entities.business.diary;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;
import java.util.Date;

/*
 * ЗАМЕТКИ
 * 
 * Порядок создания класса:
 * 1. Создать все необходимые private-поля.
 * 2. Создать public static-методы вида checkField(value), реализующие валидацию.
 * 3. Создать get-методы.
 * 4. Создать set-методы, проводящие валидацию check-методом. Если провалилась, выбросить исключение IllegalArgumentException
 * 
 * Порядок использования:
 * 1. Изменить значение с помощью set-метода.
 * 2. Быть готовым поймать исключение и обработать его на frontend'е.
 */

public abstract class DiaryRecord implements Serializable
{
	private static final long	serialVersionUID	= 1L;

	@JsonProperty("time")
	@JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
	private Date time;

	// ================================ VALIDATORS ================================

	public static boolean checkTime(Date time)
	{
		return (time != null);
	}

	// ================================ GET / SET ================================

	public Date getTime()
	{
		return time;
	}

	public void setTime(Date time)
	{
		if (!checkTime(time))
		{
			throw new IllegalArgumentException("DiaryRecord: неверное значение поля Time (" + time + ")");
		}

		this.time = time;
	}

	@JsonProperty("type")
	public abstract String getType();
}
