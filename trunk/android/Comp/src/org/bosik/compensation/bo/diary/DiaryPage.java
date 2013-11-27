package org.bosik.compensation.bo.diary;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.diary.records.DiaryRecord;
import org.bosik.compensation.utils.Utils;
import android.util.Log;

public class DiaryPage implements RecordChangeListener
{
	private static final String	TAG	= DiaryPage.class.getSimpleName();

	// TODO: тестировать

	public static enum EventType
	{
		ADD, MODIFY, REMOVE
	}

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

	private void resort()
	{
		Collections.sort(items, new RecordComparator());
	}

	private void updatePostprand()
	{
		// TODO
	}

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
			// TODO: change all IllegalArgumentExceptions to NullPointerExceptions where need
			throw new NullPointerException("Record can't be null");
		}

		rec.setChangeListener(this);
		items.add(rec);
		handleModification(EventType.ADD, rec.getClass(), rec);

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
	 * Получает указанную запись
	 * 
	 * @param index
	 *            Индекс записи
	 * @return Запись
	 */
	public DiaryRecord get(int index)
	{
		return items.get(index);
	}

	/**
	 * Удаляет запись с указанным индексом
	 * 
	 * @param index
	 *            Индекс
	 */
	public void remove(int index)
	{
		handleModification(EventType.REMOVE, items.get(index).getClass(), items.get(index));
		items.remove(index);
	}

	public void clear()
	{
		items.clear();
	}

	// -------- СОБЫТИЕ МОДИФИКАЦИИ --------

	@Override
	public void changed(Class<? extends DiaryRecord> recClass, DiaryRecord recInstance)
	{
		handleModification(EventType.MODIFY, recClass, recInstance);
	}

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
	private void handleModification(EventType eventType, Class<? extends DiaryRecord> recClass, DiaryRecord recInstance)
	{
		// Log.d(TAG, "changed()");

		// бесшумный режим
		if (silentMode)
		{
			Log.v(TAG, "changed(): silent mode is on");
			return;
		}

		// обновляем данные о версии и timeStamp
		timeStamp = Utils.now(); // TODO: use UTC time, not local one
		version++;

		// принимаем коррекционные меры
		if (recInstance != null)
		{
			if (items.indexOf(recInstance) != -1)
			{
				resort();
			}
		}

		updatePostprand();
	}
}