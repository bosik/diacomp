package org.bosik.compensation.bo.diary;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.exceptions.DuplicateException;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.utils.Utils;
import android.util.Log;

public class DiaryPage
{
	private static final String								TAG			= DiaryPage.class.getSimpleName();

	// TODO: test

	// ===================================== ПОЛЯ =====================================

	private Date											date;
	private Date											timeStamp;
	private int												version;
	private final List<Versioned<? extends DiaryRecord>>	items		= new ArrayList<Versioned<? extends DiaryRecord>>();

	private transient boolean								silentMode	= false;

	// ============================== ВНУТРЕННИЕ МЕТОДЫ ==============================

	private class RecordComparator implements Comparator<Versioned<? extends DiaryRecord>>
	{
		@Override
		public int compare(Versioned<? extends DiaryRecord> lhs, Versioned<? extends DiaryRecord> rhs)
		{
			return lhs.getData().getTime() - rhs.getData().getTime();
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
		timeStamp = Utils.now();
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

	public boolean getSilentMode()
	{
		return silentMode;
	}

	public void setSilentMode(boolean silentMode)
	{
		this.silentMode = silentMode;
	}

	// -------- РАБОТА С ЗАПИСЯМИ --------

	/**
	 * Добавляет запись в дневник
	 * 
	 * @param rec
	 *            Запись
	 * @return Индекс созданной записи на странице
	 */
	public int add(Versioned<? extends DiaryRecord> rec) throws DuplicateException
	{
		if (rec == null)
		{
			throw new NullPointerException("Record can't be null");
		}

		// TODO: add duplication check

		items.add(rec);
		changed();

		return items.indexOf(rec);
	}

	public int add(DiaryRecord rec) throws DuplicateException
	{
		if (rec == null)
		{
			throw new NullPointerException("Record can't be null");
		}

		return add(new Versioned<DiaryRecord>(rec));
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
	public Versioned<? extends DiaryRecord> get(int index)
	{
		// TODO: get by ID, not index
		try
		{
			return items.get(index).clone();
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	/**
	 * Returns record by id
	 * 
	 * @param id
	 * @return Diary record
	 */
	public Versioned<? extends DiaryRecord> get(String id) throws ItemNotFoundException
	{
		int index = getIndexById(id);
		if (index > -1)
		{
			try
			{
				return items.get(index).clone();
			}
			catch (CloneNotSupportedException e)
			{
				throw new RuntimeException(e);
			}
		}
		else
		{
			throw new ItemNotFoundException(id);
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
	public void remove(String id) throws ItemNotFoundException
	{
		int index = getIndexById(id);
		if (index > -1)
		{
			remove(index);
		}
		else
		{
			throw new ItemNotFoundException(id);
		}
	}

	public void clear()
	{
		items.clear();
	}

	public void update(Versioned<? extends DiaryRecord> rec) throws ItemNotFoundException
	{
		int index = getIndexById(rec.getId());
		if (index > -1)
		{
			try
			{
				items.set(index, rec.clone());
				changed();
			}
			catch (CloneNotSupportedException e)
			{
				throw new RuntimeException(e);
			}
		}
		else
		{
			throw new ItemNotFoundException(rec.getId());
		}
	}
}