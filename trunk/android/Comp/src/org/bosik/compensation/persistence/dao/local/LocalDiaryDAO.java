package org.bosik.compensation.persistence.dao.local;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.face.BuildConfig;
import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.local.utils.DiaryContentProvider;
import org.bosik.compensation.persistence.serializers.diary.DiaryPagePlainSerializer;
import org.bosik.compensation.utils.Utils;
import android.app.Activity;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;

public class LocalDiaryDAO implements DiaryDAO
{
	/* ============================ КОНСТАНТЫ ============================ */

	// private static final String TAG = LocalDiaryDAO.class.getSimpleName();

	public static final int	AUTO_CHECK	= 0;
	public static final int	SURE_INSERT	= 1;
	public static final int	SURE_UPDATE	= 2;

	/* ============================ ПОЛЯ ============================ */

	private ContentResolver	aResolver;

	/* ======================= ВНУТРЕННИЕ МЕТОДЫ ========================= */

	/**
	 * Ищет страницу в базе
	 * 
	 * @param date
	 *            Дата
	 * @return Страница (если не найдена, возвращается null)
	 */
	private DiaryPage findPage(Date date)
	{
		if (null == date)
		{
			throw new NullPointerException("Date can't be null");
			// Log.i(TAG,"getPage(): date is " + date.toString());
		}

		// формируем параметры
		String[] mProj = { DiaryContentProvider.COLUMN_DATE, DiaryContentProvider.COLUMN_TIMESTAMP,
				DiaryContentProvider.COLUMN_VERSION, DiaryContentProvider.COLUMN_PAGE };
		String mSelectionClause = DiaryContentProvider.COLUMN_DATE + " = ?";
		String[] mSelectionArgs = { Utils.formatDate(date) };
		String mSortOrder = null;

		// выполняем запрос
		Cursor cursor = aResolver.query(DiaryContentProvider.CONTENT_URI, mProj, mSelectionClause, mSelectionArgs,
				mSortOrder);

		// анализируем ответ
		if (cursor == null)
		{
			if (BuildConfig.DEBUG)
			{
				throw new RuntimeException("Cursor is null");
			}
			else
			{
				return null;
			}
		}

		if (cursor.getCount() < 1)
		{
			// ничего не нашли
			return null;
		}

		if ((cursor.getCount() > 1) && (BuildConfig.DEBUG))
		{
			// на самом деле мы производим выборку по полю Date, которое при создании
			// таблицы имеет атрибут UNIQUE, так что такого в принципе быть не должно

			throw new RuntimeException("Several pages are founded");
		}

		int indexTimeStamp = cursor.getColumnIndex(DiaryContentProvider.COLUMN_TIMESTAMP);
		int indexVersion = cursor.getColumnIndex(DiaryContentProvider.COLUMN_VERSION);
		int indexPage = cursor.getColumnIndex(DiaryContentProvider.COLUMN_PAGE);
		cursor.moveToNext();

		try
		{
			Date timeStamp = Utils.parseTime(cursor.getString(indexTimeStamp));
			int version = cursor.getInt(indexVersion);
			String source = cursor.getString(indexPage);

			DiaryPage diaryPage = new DiaryPage(date, timeStamp, version);
			DiaryPagePlainSerializer.readContent(source, diaryPage);
			return diaryPage;
		}
		catch (ParseException e)
		{
			throw new RuntimeException("Can't parse timestamp", e);
		}
	}

	/**
	 * Отправляет страницу дневника в базу (<i>небезопасная версия для оптимизации</i>)
	 * 
	 * @param diaryPage
	 *            Отправляемая страница
	 * @param CheckMode
	 *            Режим проверки: <br/>
	 *            <i>AUTO_CHECK</i> — Автоматический выбор insert() или update()<br/>
	 *            <i>SURE_INSERT</i> — Добавление без предварительной проверки <br/>
	 *            <i>SURE_UPDATE</i> — Обновление без предварительной проверки
	 */
	private void postPageExt(DiaryPage diaryPage, int CheckMode)
	{
		// Log.i(TAG, "PostPage()");

		String code = DiaryPagePlainSerializer.writeContent(diaryPage);
		// Log.v(TAG, "PostPage(): date is " + diaryPage.getDate().toString());
		// Log.v(TAG,"PostPage(): page is " + code);

		boolean exists;

		switch (CheckMode)
		{
			case SURE_UPDATE:
				exists = true;
				break;
			case SURE_INSERT:
				exists = false;
				break;
			case AUTO_CHECK:
				exists = (findPage(diaryPage.getDate()) != null);
				break;
			default:
				throw new IllegalArgumentException("Incorrect CheckMode value (" + CheckMode + ")");
		}

		ContentValues mNewValues = new ContentValues();

		mNewValues.put(DiaryContentProvider.COLUMN_TIMESTAMP, Utils.formatTime(diaryPage.getTimeStamp()));
		mNewValues.put(DiaryContentProvider.COLUMN_VERSION, diaryPage.getVersion());
		mNewValues.put(DiaryContentProvider.COLUMN_PAGE, code);

		if (exists)
		{
			// Log.d(TAG, "PostPage(): page exists, updating...");
			aResolver.update(DiaryContentProvider.CONTENT_URI, mNewValues, "Date = ?",
					new String[] { Utils.formatDate(diaryPage.getDate()) });
		}
		else
		{
			// Log.d(TAG, "PostPage(): page doesn't exist, inserting...");
			mNewValues.put(DiaryContentProvider.COLUMN_DATE, Utils.formatDate(diaryPage.getDate()));
			aResolver.insert(DiaryContentProvider.CONTENT_URI, mNewValues);
		}
	}

	/* ============================ ВНЕШНИЕ МЕТОДЫ ============================ */

	/**
	 * Конструктор
	 * 
	 * @param resolver
	 *            Контент-приёмник. Можно получить с помощью метода
	 *            {@link Activity#getContentResolver()}
	 */
	public LocalDiaryDAO(ContentResolver resolver)
	{
		if (null == resolver)
		{
			throw new NullPointerException("Content Resolver can't be null");
		}
		aResolver = resolver;
	}

	// -------------------- API --------------------

	@Override
	public List<PageVersion> getModList(Date time)
	{
		// формируем параметры
		String[] mProjection = { DiaryContentProvider.COLUMN_DATE, DiaryContentProvider.COLUMN_VERSION };
		String mSelectionClause = DiaryContentProvider.COLUMN_TIMESTAMP + " > ?";
		String[] mSelectionArgs = { Utils.formatTime(time) };
		String mSortOrder = null;

		// выполняем запрос
		Cursor mCursor = aResolver.query(DiaryContentProvider.CONTENT_URI, mProjection, mSelectionClause,
				mSelectionArgs, mSortOrder);

		if (mCursor != null)
		{
			int indexDate = mCursor.getColumnIndex(DiaryContentProvider.COLUMN_DATE);
			int indexVersion = mCursor.getColumnIndex(DiaryContentProvider.COLUMN_VERSION);
			List<PageVersion> res = new ArrayList<PageVersion>();

			while (mCursor.moveToNext())
			{
				try
				{
					Date date = Utils.parseDate(mCursor.getString(indexDate));
					int version = mCursor.getInt(indexVersion);
					res.add(new PageVersion(date, version));
				}
				catch (ParseException e)
				{
					// THINK: как правильно решать эту ситуацию?
					throw new RuntimeException(e);
				}
			}

			return res;
		}
		else
		{
			throw new NullPointerException("Cursor is null");
		}
	}

	@Override
	public List<DiaryPage> getPages(List<Date> dates)
	{
		List<DiaryPage> result = new ArrayList<DiaryPage>();
		for (Date date : dates)
		{
			result.add(getPage(date));
		}
		return result;
	}

	@Override
	public boolean postPages(List<DiaryPage> pages)
	{
		boolean result = true;
		for (DiaryPage page : pages)
		{
			if (!postPage(page))
			{
				result = false;
			}
		}
		return result;
	}

	@Override
	public DiaryPage getPage(Date date)
	{
		DiaryPage page = findPage(date);
		if (page != null)
		{
			return page;
		}
		else
		{
			return new DiaryPage(date, Utils.now(), 0);
		}
	}

	@Override
	public boolean postPage(DiaryPage diaryPage)
	{
		/**
		 * Аналог postPageExt(page, AUTO_CHECK). Предварительно выполняется проверка и в зависимости
		 * от наличия записи в БД выполняется insert() или update()
		 */
		postPageExt(diaryPage, AUTO_CHECK);
		return true;
	}
}
