package org.bosik.compensation.persistence.dao.local;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.face.BuildConfig;
import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.local.utils.DiaryContentProvider;
import org.bosik.compensation.persistence.exceptions.CommonDAOException;
import org.bosik.compensation.persistence.exceptions.StoreException;
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

	/* ============================ ПОЛЯ ============================ */

	private ContentResolver	resolver;

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
		try
		{
			if (null == date)
			{
				throw new NullPointerException("Date can't be null");
			}

			// формируем параметры
			String[] mProj = { DiaryContentProvider.COLUMN_DIARY_DATE, DiaryContentProvider.COLUMN_DIARY_TIMESTAMP,
					DiaryContentProvider.COLUMN_DIARY_VERSION, DiaryContentProvider.COLUMN_DIARY_PAGE };
			String mSelectionClause = DiaryContentProvider.COLUMN_DIARY_DATE + " = ?";
			String[] mSelectionArgs = { Utils.formatDate(date) };
			String mSortOrder = null;

			// выполняем запрос
			Cursor cursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, mProj, mSelectionClause,
					mSelectionArgs, mSortOrder);

			// анализируем ответ
			if (cursor == null)
			{
				throw new NullPointerException("Cursor is null");
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

				throw new IllegalStateException("Several pages found");
			}

			int indexTimeStamp = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_TIMESTAMP);
			int indexVersion = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_VERSION);
			int indexPage = cursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_PAGE);
			cursor.moveToNext();

			Date timeStamp = Utils.parseTimeUTC(cursor.getString(indexTimeStamp));
			int version = cursor.getInt(indexVersion);
			String source = cursor.getString(indexPage);

			DiaryPage diaryPage = new DiaryPage(date, timeStamp, version);
			DiaryPagePlainSerializer.readContent(source, diaryPage);
			return diaryPage;
		}
		catch (Exception e)
		{
			throw new CommonDAOException(e);
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
		this.resolver = resolver;
	}

	// -------------------- API --------------------

	@Override
	public List<PageVersion> getModList(Date time)
	{
		// формируем параметры
		String[] mProjection = { DiaryContentProvider.COLUMN_DIARY_DATE, DiaryContentProvider.COLUMN_DIARY_VERSION };
		String mSelectionClause = DiaryContentProvider.COLUMN_DIARY_TIMESTAMP + " > ?";
		String[] mSelectionArgs = { Utils.formatTimeUTC(time) };
		String mSortOrder = null;

		// выполняем запрос
		Cursor mCursor = resolver.query(DiaryContentProvider.CONTENT_DIARY_URI, mProjection, mSelectionClause,
				mSelectionArgs, mSortOrder);

		if (mCursor != null)
		{
			int indexDate = mCursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_DATE);
			int indexVersion = mCursor.getColumnIndex(DiaryContentProvider.COLUMN_DIARY_VERSION);
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
					throw new CommonDAOException(e);
				}
			}

			return res;
		}
		else
		{
			throw new CommonDAOException(new NullPointerException("Cursor is null"));
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
	public void postPages(List<DiaryPage> pages)
	{
		for (DiaryPage page : pages)
		{
			postPage(page);
		}
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
	public void postPage(DiaryPage diaryPage)
	{
		try
		{
			String code = DiaryPagePlainSerializer.writeContent(diaryPage);
			boolean exists = (findPage(diaryPage.getDate()) != null);

			ContentValues newValues = new ContentValues();
			newValues.put(DiaryContentProvider.COLUMN_DIARY_DATE, Utils.formatDate(diaryPage.getDate()));
			newValues.put(DiaryContentProvider.COLUMN_DIARY_TIMESTAMP, Utils.formatTimeUTC(diaryPage.getTimeStamp()));
			newValues.put(DiaryContentProvider.COLUMN_DIARY_VERSION, diaryPage.getVersion());
			newValues.put(DiaryContentProvider.COLUMN_DIARY_PAGE, code);

			if (exists)
			{
				String[] args = new String[] { Utils.formatDate(diaryPage.getDate()) };
				resolver.update(DiaryContentProvider.CONTENT_DIARY_URI, newValues, "Date = ?", args);
			}
			else
			{
				resolver.insert(DiaryContentProvider.CONTENT_DIARY_URI, newValues);
			}
		}
		catch (Exception e)
		{
			throw new StoreException(e);
		}
	}
}
