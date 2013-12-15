package org.bosik.compensation.bo.diary;

import java.io.Serializable;
import org.bosik.compensation.bo.basic.TrueCloneable;
import org.bosik.compensation.utils.Utils;

/* ЗАМЕТКИ 
 * 
 * Порядок создания класса:
 * 		1. Создать все необходимые private-поля.
 * 		2. Создать public static-методы вида checkField(value), реализующие валидацию.
 * 		3. Создать get-методы.
 * 		4. Создать set-методы, проводящие валидацию check-методом. Если провалилась, выбросить исключение IllegalArgumentException
 * 		5. Создать валидатор, проверяющий сразу все поля (????????)
 * 		6. Создать конструктор, принимающий сразу все поля и устанавливающий их с помощью set-методов.
 * 
 * Порядок использования:
 * 		1. Изменить значение с помощью set-метода.
 * 		2. Быть готовым поймать исключение и обработать его на frontend'е.
 */

public abstract class DiaryRecord implements TrueCloneable, Serializable
{
	private static final long	serialVersionUID	= 1L;
	// private static final String TAG = "DiaryRecord";

	// данные
	private int					time;

	public DiaryRecord()
	{

	}

	// ============================== CLONE ==============================

	@Override
	public DiaryRecord clone()
	{
		try
		{
			return (DiaryRecord) super.clone();
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	// ================================ VALIDATORS ================================

	public static boolean checkTime(int time)
	{
		return (time >= 0) && (time < Utils.SecPerDay);
	}

	// ================================ GET / SET ================================

	public int getTime()
	{
		return time;
	}

	public void setTime(int time)
	{
		if (!checkTime(time))
		{
			throw new IllegalArgumentException("DiaryRecord: неверное значение поля Time (" + time + ")");
		}

		this.time = time;
	}
}
