package org.bosik.compensation.bo.diary;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.diary.records.DiaryRecord;
import org.bosik.compensation.utils.Utils;
import android.util.Log;

public class DiaryPage
{
	private static final String		TAG			= DiaryPage.class.getSimpleName();

	// TODO: тестировать

	// ===================================== ПОЛЯ =====================================

	private Date					date		= null;
	private Date					timeStamp	= null;
	private int						version		= 0;
	private final List<DiaryRecord>	items		= new ArrayList<DiaryRecord>();

	public boolean					silentMode	= false;

	// ============================== ВНУТРЕННИЕ МЕТОДЫ ==============================

	private class RecordComparator implements Comparator<DiaryRecord>
	{
		@Override
		public int compare(DiaryRecord lhs, DiaryRecord rhs)
		{
			return lhs.getTime() - rhs.getTime();
		}
	}

	private void updatePostprand()
	{
		// TODO
	}

	private int getIndexById(String id)
	{
		for (int i = 0; i < items.size(); i++)
		{
			if (id.equals(items.get(i).getId()))
			{
				return i;
			}
		}
		return -1;
	}

	private void changed()
	{
		if (silentMode)
		{
			Log.v(TAG, "changed(): silent mode is on");
			return;
		}

		Collections.sort(items, new RecordComparator());
		updatePostprand();
		timeStamp = Utils.now(); // TODO: use UTC time, not local one
		version++;
	}

	// private DiaryRecord safeClone(DiaryRecord rec)
	// {
	// try
	// {
	// return (DiaryRecord) rec.clone();
	// }
	// catch (CloneNotSupportedException e)
	// {
	// throw new RuntimeException(e);
	// }
	// }

	// ================================ ВНЕШНИЕ МЕТОДЫ ================================

	// -------- КОНСТРУКТОРЫ --------

	/**
	 * Пустой конструктор
	 */
	public DiaryPage()
	{

	}

	/**
	 * Конструктор на случай, если заголовок уже распарсен
	 * 
	 * @param date
	 *            Дата
	 * @param timeStamp
	 *            Время модификации
	 * @param version
	 *            Номер версии
	 */
	public DiaryPage(Date date, Date timeStamp, int version)
	{
		// проверки
		if (date == null)
		{
			throw new NullPointerException("Date can't be null");
		}
		if (timeStamp == null)
		{
			throw new NullPointerException("TimeStamp can't be null");
		}
		if (version < 0)
		{
			throw new IllegalArgumentException("Version can't be negative");
		}

		// чтение
		this.date = date;
		this.timeStamp = timeStamp;
		this.version = version;
	}

	// -------- РАБОТА С ПОЛЯМИ --------

	public Date getDate()
	{
		return date;
	}

	public void setDate(Date date)
	{
		this.date = date;
	}

	public Date getTimeStamp()
	{
		return timeStamp;
	}

	public void setTimeStamp(Date timeStamp)
	{
		this.timeStamp = timeStamp;
	}

	public int getVersion()
	{
		return version;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	// -------- РАБОТА С ЗАПИСЯМИ --------

	/**
	 * Добавляет запись в дневник
	 * 
	 * @param rec
	 *            Запись
	 * @return Индекс созданной записи на странице
	 */
	public int add(DiaryRecord rec)
	{
		if (rec == null)
		{
			throw new NullPointerException("Record can't be null");
		}

		items.add(rec);
		changed();

		return items.indexOf(rec);
	}

	/**
	 * Получает количество записей
	 * 
	 * @return Количество записей
	 */
	public int count()
	{
		return items.size();
	}

	/**
	 * Returns record by index
	 * 
	 * @param index
	 * @return
	 */
	public DiaryRecord get(int index)
	{
		// TODO: get by ID, not index
		return items.get(index).clone();
	}

	/**
	 * Returns record by id
	 * 
	 * @param id
	 * @return Diary record if found, null otherwise
	 */
	public DiaryRecord get(String id)
	{
		int index = getIndexById(id);
		if (index > -1)
		{
			// try
			// {
			return items.get(index).clone();
			// }
			// catch (CloneNotSupportedException e)
			// {
			// throw new RuntimeException(e);
			// }
		}
		else
		{
			return null;
		}
	}

	/**
	 * Removes record by index
	 * 
	 * @param index
	 */
	public void remove(int index)
	{
		items.remove(index);
		changed();
	}

	/**
	 * Removes record by ID
	 * 
	 * @param id
	 */
	public void remove(String id)
	{
		int index = getIndexById(id);
		if (index > -1)
		{
			remove(index);
		}
		else
		{
			// TODO; change to NotFoundException()
			throw new RuntimeException();
		}
	}

	public void clear()
	{
		items.clear();
	}

	// -------- СОБЫТИЕ МОДИФИКАЦИИ --------

	/**
	 * Событие изменения страницы, вызывается записями страницы
	 * 
	 * @param eventType
	 *            Тип события
	 * @param recClass
	 *            Класс изменившейся записи. Полезен в случае, если запись оповещает о своём
	 *            удалении (WAT?)
	 * @param recInstance
	 *            Экземпляр оповещающей записи (а-ля sender)
	 */
	// private void handleModification(EventType eventType, Class<? extends DiaryRecord> recClass,
	// DiaryRecord recInstance)
	// {
	// // Log.d(TAG, "changed()");
	//
	// // бесшумный режим
	// if (silentMode)
	// {
	// Log.v(TAG, "changed(): silent mode is on");
	// return;
	// }
	//
	// // обновляем данные о версии и timeStamp
	// timeStamp = Utils.now(); // TODO: use UTC time, not local one
	// version++;
	//
	// // принимаем коррекционные меры
	// if (recInstance != null)
	// {
	// if (items.indexOf(recInstance) != -1)
	// {
	// resort();
	// }
	// }
	//
	// updatePostprand();
	// }

	public void update(DiaryRecord rec)
	{
		int index = getIndexById(rec.getId());
		if (index > -1)
		{
			// try
			// {
			items.set(index, rec.clone());
			// }
			// catch (CloneNotSupportedException e)
			// {
			// throw new RuntimeException(e);
			// }
		}
		else
		{
			// TODO; change to NotFoundException()
			throw new RuntimeException();
		}
	}
}