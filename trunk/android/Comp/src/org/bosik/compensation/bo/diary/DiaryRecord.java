package org.bosik.compensation.bo.diary;

import java.io.Serializable;
import java.util.Date;

/* ЗАМЕТКИ 
 * 
 * Порядок создания класса:
 * 		1. Создать все необходимые private-поля.
 * 		2. Создать public static-методы вида checkField(value), реализующие валидацию.
 * 		3. Создать get-методы.
 * 		4. Создать set-методы, проводящие валидацию check-методом. Если провалилась, выбросить исключение IllegalArgumentException
 * 
 * Порядок использования:
 * 		1. Изменить значение с помощью set-метода.
 * 		2. Быть готовым поймать исключение и обработать его на frontend'е.
 */

public abstract class DiaryRecord implements Serializable
{
	private static final long	serialVersionUID	= 1L;

	private Date				time;

	// ================================ VALIDATORS ================================

	// TODO: trivial (always returns true)
	public static boolean checkTime(Date time)
	{
		// return (time >= 0) && (time < Utils.SecPerDay);
		return true;
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
}
