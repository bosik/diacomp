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
	 * Получает несколько страниц дневника из БД. Если страница не существует, она будет создана.
	 * 
	 * @param dates
	 *            Даты, для которых необходимо получить страницы
	 * @return Страницы
	 * @see #getPage(Date)
	 */
	public List<DiaryPage> getPages(List<Date> dates);

	/**
	 * Отправляет несколько страниц дневника в БД.
	 * 
	 * @param pages
	 *            Страницы
	 * @return Успешность отправки
	 * @see #postPage(DiaryPage)
	 */
	public boolean postPages(List<DiaryPage> pages);

	/**
	 * Получает одну страницу из БД. Для получения большего количества данных используйте
	 * {@link #getPages(List)}.
	 * 
	 * @param date
	 *            Дата
	 * @return Страница
	 */
	public DiaryPage getPage(Date date);

	/**
	 * Отправляет одну страницу в БД. Для отправки большего количества данных используйте
	 * {@link #postPages(List)}.
	 * 
	 * @param date
	 *            Дата
	 * @return Страница
	 */
	public boolean postPage(DiaryPage page);
}
