package org.bosik.compensation.persistence.entity.diary;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.persistence.entity.common.FoodMassed;
import org.bosik.compensation.persistence.entity.diary.records.BloodRecord;
import org.bosik.compensation.persistence.entity.diary.records.DiaryRecord;
import org.bosik.compensation.persistence.entity.diary.records.InsRecord;
import org.bosik.compensation.persistence.entity.diary.records.MealRecord;
import org.bosik.compensation.persistence.entity.diary.records.NoteRecord;
import org.bosik.compensation.utils.Utils;
import android.util.Log;

public class DiaryPage implements DiaryChangeListener
{
	// отладочная печать
	private static final String		TAG			= "DiaryPage";

	// TODO: тестировать

	// ===================================== ПОЛЯ =====================================

	private Date					date		= null;
	private Date					timeStamp	= null;
	private int						version		= 0;
	private final List<DiaryRecord>	items		= new ArrayList<DiaryRecord>();

	private boolean					silentMode	= false;

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

	// ================================ ВНЕШНИЕ МЕТОДЫ ================================

	// -------- КОНСТРУКТОРЫ --------

	/**
	 * Пустой конструктор
	 */
	public DiaryPage()
	{

	}

	/**
	 * Конструктор на случай, если ничего не распарсено
	 * 
	 * @param source
	 *            Источник
	 * @param fullCode
	 *            Исходный код страницы
	 */
	public DiaryPage(String fullCode)
	{
		readFull(fullCode);
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
	 * @param contentCode
	 *            Исходный код содержимого
	 */
	public DiaryPage(Date date, Date timeStamp, int version, final String contentCode)
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
		if (contentCode == null)
		{
			throw new NullPointerException("ContentCode can't be null");
		}

		// чтение
		this.date = date;
		this.timeStamp = timeStamp;
		this.version = version;
		readContent(contentCode);
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
			throw new IllegalArgumentException("Record can't be null");
		}

		rec.setChangeListener(this);
		items.add(rec);
		changed(EventType.ADD, rec.getClass(), rec);

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
		changed(EventType.REMOVE, items.get(index).getClass(), items.get(index));
		items.remove(index);
	}

	// -------- СОБЫТИЕ МОДИФИКАЦИИ --------

	/**
	 * Событие изменения страницы, вызывается записями страницы
	 * 
	 * @param eventType
	 *            Тип события
	 * @param recClass
	 *            Класс изменившейся записи. Полезен в случае, если запись оповещает о своём
	 *            удалении
	 * @param recInstance
	 *            Экземпляр оповещающей записи (а-ля sender)
	 */
	@Override
	public void changed(EventType eventType, Class<? extends DiaryRecord> recClass, DiaryRecord recInstance)
	{
		// Log.d(TAG, "changed()");

		// бесшумный режим
		if (silentMode)
		{
			Log.v(TAG, "changed(): silent mode is on");
			return;
		}

		Log.i(TAG, "changed(): version have been updated");

		// обновляем данные о версии и timeStamp
		timeStamp = Utils.now();
		version++;

		// принимаем коррекционные меры
		if (recInstance != null)
		{
			int index = items.indexOf(recInstance);
			if (index != -1)
			{
				resort();
			}
		}
	}

	// -------- ВВОД-ВЫВОД --------

	/**
	 * Разбирает заголовок страницы из строк
	 * 
	 * @param headerCode
	 *            Строка
	 * @return Удалось ли прочитать заголовок
	 */
	public boolean readHeader(final String headerCode)
	{
		// Log.i(TAG, "readHeader(): " + headerCode);
		String[] p = null;
		try
		{
			p = headerCode.split("\\|");
			date = Utils.parseDate(p[0].substring(4, 14));
			timeStamp = Utils.parseTime(p[1]);
			version = Integer.parseInt(p[2]);
			return true;
		}
		catch (ParseException e)
		{
			Log.e(TAG, "readHeader(): headerCode = '" + headerCode + "'");
			Log.e(TAG, "readHeader(): p.length = " + p.length);
			for (int i = 0; i < p.length; i++)
			{
				Log.e(TAG, "readHeader(): p[" + i + "] = '" + p[i] + "'");
			}
			e.printStackTrace();
			return false;
		}
	}

	/**
	 * Создаёт текстовое представление заголовка страницы
	 * 
	 * @return Текстовое представление заголовка страницы
	 */
	public String writeHeader()
	{
		return "=== " + Utils.formatDate(date) + " ===|" + Utils.formatTime(timeStamp) + "|" + String.valueOf(version);
	}

	/**
	 * Загружает страницу из её текстового представления. При возникновении ошибок информирует через
	 * LogCat и возвращая false, однако разбор продолжает (fail-soft)
	 * 
	 * @param contentCode
	 *            Исходный код страницы
	 * @return Удалось ли успешно прочитать страницу
	 */
	public boolean readContent(final String contentCode)
	{
		Log.i(TAG, "readContent(), silentMode before IS " + silentMode);
		items.clear();

		if (null == contentCode)
		{
			throw new NullPointerException("Source can't be null");
		}
		if (contentCode.trim().equals(""))
		{
			return true;
		}

		boolean result = true;
		String[] Lines = contentCode.split("\n");
		MealRecord activeMeal = null;
		boolean oldSilentMode = silentMode;
		silentMode = true;

		// Важно: в конце каждого элемента Lines[i] есть символ '\n' (код 13)
		for (int i = 0; i < Lines.length; i++)
		{
			if (!Lines[i].trim().equals(""))
			{
				try
				{
					switch (Lines[i].charAt(0))
					{
						case '*':
						{
							int TempTime;
							double TempValue;
							int TempFinger;
							int v = Lines[i].indexOf('|');

							// без указания пальца
							if (v == -1)
							{
								TempTime = Utils.strToTime(Lines[i].substring(1, 6));
								TempValue = Utils.parseDouble(Lines[i].substring(7));
								TempFinger = -1;
							}
							else
							// с указанием пальца
							{
								TempTime = Utils.strToTime(Lines[i].substring(1, 6));
								TempValue = Utils.parseDouble(Lines[i].substring(7, v));
								TempFinger = Integer.parseInt(Lines[i].substring(v + 1).trim());
							}

							add(new BloodRecord(TempTime, TempValue, TempFinger));

							activeMeal = null;
							break;
						}
						case '-':
						{
							int TempTime = Utils.strToTime(Lines[i].substring(1, 6));
							double TempValue = Utils.parseDouble(Lines[i].substring(7).trim());
							add(new InsRecord(TempTime, TempValue));

							activeMeal = null;
							break;
						}
						case ' ':
						{
							int TempTime = Utils.strToTime(Lines[i].substring(1, 6));
							boolean TempShort = Lines[i].endsWith("s");

							activeMeal = new MealRecord(TempTime, TempShort);
							add(activeMeal);
							break;
						}
						case '#':
						{
							if (activeMeal != null)
							{
								FoodMassed food = new FoodMassed();
								food.read(Lines[i].substring(1));
								activeMeal.add(food);
							}
							else
							{
								Log.e(TAG, "DiaryPage.readContent(): food without meal declaration ignored: "
										+ Lines[i]);
								result = false;
							}
							break;
						}
						case '%':
						{
							// TODO: откуда в конце строки лишний символ?
							int TempTime = Utils.strToTime(Lines[i].substring(1, 6));
							String TempValue = Lines[i].substring(7);
							add(new NoteRecord(TempTime, TempValue));
							activeMeal = null;
							break;
						}
						default:
						{
							Log.e(TAG, "DiaryPage.readContent(): Unknown formatted line ignored: " + Lines[i]);
							result = false;
						}
					}
				}
				// защищаем внутри каждой итерации для повышения устойчивости
				catch (Exception e)
				{
					Log.e(TAG, "DiaryPage.readContent(): Error parsing line #" + i + ": '" + Lines[i] + "'");
					Log.e(TAG, "DiaryPage.readContent(): with message " + e.getLocalizedMessage());
					Log.e(TAG, "DiaryPage.readContent(): contentCode = '" + contentCode + "'");
					result = false;
				}
			}
		}

		silentMode = oldSilentMode;
		Log.i(TAG, "readContent(), silentMode after IS " + silentMode);
		return result;
	}

	/**
	 * Создаёт текстовое представление содержимого страницы
	 * 
	 * @return Текстовое представление содержимого страницы
	 */
	public String writeContent()
	{
		String result = "";
		Class<? extends DiaryRecord> c;

		for (int i = 0; i < items.size(); i++)
		{
			c = items.get(i).getClass();

			if (c == BloodRecord.class)
			{
				BloodRecord temp = (BloodRecord) items.get(i);
				result += '*' + Utils.timeToStr(temp.getTime()) + ' ' + String.valueOf(temp.getValue()) + '|'
						+ String.valueOf(temp.getFinger()) + '\n';
			}
			else
				if (c == InsRecord.class)
				{
					InsRecord temp = (InsRecord) items.get(i);
					result += '-' + Utils.timeToStr(temp.getTime()) + ' ' + String.valueOf(temp.getValue()) + '\n';
				}
				else
					if (c == MealRecord.class)
					{
						MealRecord temp = (MealRecord) items.get(i);

						result += ' ' + Utils.timeToStr(temp.getTime());
						if (temp.getShortMeal())
						{
							result += "s";
						}
						result += "\n";

						for (int k = 0; k < temp.size(); k++)
						{
							result += '#' + temp.get(k).write() + '\n';
						}
					}
					else
						if (c == NoteRecord.class)
						{
							NoteRecord temp = (NoteRecord) items.get(i);
							result += '%' + Utils.timeToStr(temp.getTime()) + ' ' + temp.getText() + '\n';
						}
		}
		return result;
	}

	private void readFull(final String fullCode)
	{
		int n = fullCode.indexOf("\n");
		if (n > -1)
		{
			readHeader(fullCode.substring(0, n));
			readContent(fullCode.substring(n + 1));
		}
		else
		{
			readHeader(fullCode);
		}
	}

	public String writeFull()
	{
		return writeHeader() + "\n" + writeContent();
	}

	/**
	 * Читает произвольное число страниц из текстового потока
	 * 
	 * @param s
	 *            Строка с данными
	 * @param source
	 *            Источник
	 * @return Прочитанный массив страниц
	 */
	public static List<DiaryPage> multiRead(final String s)
	{
		List<DiaryPage> pages = new ArrayList<DiaryPage>();
		String[] lines = s.split("\n");
		String buf = "";

		for (int i = 0; i < lines.length; i++)
		{
			if (!lines[i].equals(""))
			{
				if (lines[i].charAt(0) == '=')
				{
					if (!buf.trim().equals(""))
					{
						pages.add(new DiaryPage(buf));
						buf = "";
					}
				}
				buf += lines[i] + "\n";
			}
		}

		if (!buf.trim().equals(""))
		{
			pages.add(new DiaryPage(buf));
			buf = "";
		}

		return pages;
	}

	/**
	 * Записывает произвольное число страниц в текстовый поток
	 * 
	 * @param pages
	 *            Массив страниц
	 * @return Текст
	 */
	public static String multiWrite(List<DiaryPage> pages)
	{
		String result = "";
		for (DiaryPage page : pages)
		{
			result += page.writeFull() + "\n";
		}
		return result;
	}

}