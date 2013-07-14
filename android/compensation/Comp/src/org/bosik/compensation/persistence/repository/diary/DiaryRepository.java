package org.bosik.compensation.persistence.repository.diary;

import java.util.Date;
import java.util.List;
import org.bosik.compensation.persistence.entity.diary.DiaryPage;

/**
 * Источник данных для дневника
 * 
 * @author Bosik
 */
public interface DiaryRepository
{

	/**
	 * Хранит пары "дата : версия"
	 * 
	 * @author Bosik
	 */
	public static class PageVersion
	{
		public Date date;
		public int version;

		public PageVersion(Date date, int version)
		{
			this.date = date;
			this.version = version;
		}
	}

	/**
	 * Получает информацию о страницах, изменённых после указанного времени.
	 * 
	 * @param time
	 *            Время
	 * @return Массив пар "date:version" (см. {@link PageVersion})
	 */
	public List<PageVersion> getModList(Date time);

	/**
	 * Получает страницы дневника из БД. Если страница не существует, она должна быть создана.
	 * 
	 * @param dates
	 *            Даты, для которых необходимо получить страницы
	 * @return Страницы
	 */
	public List<DiaryPage> getPages(List<Date> dates);

	/**
	 * Отправляет страницы дневника в БД
	 * 
	 * @param pages
	 *            Страницы
	 * @return Успешность отправки
	 */
	public boolean postPages(List<DiaryPage> pages);

	/* ====================================== РЕАЛИЗАЦИИ ==================================== */

	/**
	 * Получает <b>одну</b> страницу из БД. Для запроса большего количества данных используйте
	 * getPages().
	 * 
	 * @param date
	 *            Дата
	 * @return Страница
	 */
	/*
	 * public DiaryPage getPage(Date date) { List<Date> dates = new ArrayList<Date>();
	 * dates.add(date); return getPages(dates).get(0); }
	 */

	/**
	 * Отправляет <b>одну</b> страницу в БД. Для отправки большего количества данных используйте
	 * postPages().
	 * 
	 * @param date
	 *            Дата
	 * @return Страница
	 */
	/*
	 * public boolean postPage(DiaryPage page) { List<DiaryPage> pages = new ArrayList<DiaryPage>();
	 * pages.add(page); return postPages(pages); }
	 */
}
