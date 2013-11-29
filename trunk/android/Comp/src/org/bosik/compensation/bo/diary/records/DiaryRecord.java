package org.bosik.compensation.bo.diary.records;

import org.bosik.compensation.persistence.common.Unique;
import org.bosik.compensation.utils.Utils;

/* ЗАМЕТКИ 
 * 
 * Порядок создания класса:
 * 		1. Создать все необходимые private-поля.
 * 		2. Создать public static-методы вида checkField(value), реализующие валидацию.
 * 		3. Создать get-методы.
 * 		4. Создать set-методы, проводящие валидацию check-методом.
 * 				4.0. Провести валидацию. Если провалилась, выбросить исключение IllegalArgumentException
 * 				4.1. Если нет — проверить, что новое значение отличается от старого 
 * 				4.2. Если отличается — вызвать notifyModified();
 * 		5. Создать валидатор, проверяющий сразу все поля (????????)
 * 		6. Создать конструктор, принимающий сразу все поля и устанавливающий их с помощью set-методов.
 * 
 * Порядок использования:
 * 		1. Изменить значение с помощью set-метода.
 * 		2. Быть готовым поймать исключение и обработать его на frontend'е.
 */

public abstract class DiaryRecord extends Unique
{
	private static final long	serialVersionUID	= 3497929855437219102L;

	@SuppressWarnings("unused")
	private static final String	TAG					= "DiaryRecord";

	// данные
	private int					time;

	// служебные
	// TODO: remove silent mode here, leave this feature just at DiaryPage level
	// private transient boolean silentMode = true;
	// private transient boolean modified = false;

	public DiaryRecord()
	{
		// id = Utils.
	}

	// ============================== СЛУЖЕБНЫЕ МЕТОДЫ ==============================

	// protected void notifyModified()
	// {
	// // пытаемся оповестить страницу и опустить флаг
	// if ((changeListener != null) && (!silentMode))
	// {
	// // Log.i(TAG, "notifyModified(): notifying the changeListener");
	// changeListener.changed(this.getClass(), this);
	// modified = false;
	// }
	// else
	// {
	// // запоминаем непереданное изменение и поднимаем флаг модифицированности
	// modified = true;
	//
	// /*
	// * if (silentMode) Log.v(TAG, "notifyModified(): silentMode is on"); else Log.v(TAG,
	// * "notifyModified(): parent changeListener == null");
	// */
	// }
	// }

	// public void beginUpdate()
	// {
	// silentMode = true;
	// }
	//
	// public void endUpdate()
	// {
	// silentMode = false;
	// if (modified)
	// {
	// notifyModified();
	// }
	// }

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

	// ================================ ВАЛИДАТОРЫ ================================

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
