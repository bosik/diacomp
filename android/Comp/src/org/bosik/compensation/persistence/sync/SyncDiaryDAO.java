package org.bosik.compensation.persistence.sync;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.DiaryDAO.PageVersion;

public class SyncDiaryDAO
{
	/* ============================ КОНСТАНТЫ ============================ */

	@SuppressWarnings("unused")
	private static final String	TAG	= "SyncDiaryDAO";

	/* ============================ КЛАССЫ ============================ */

	public static interface Callback
	{
		public void update_max(int max);

		public void update_progress(int progress);
	}

	/*
	 * public class NoModList1Exception extends RuntimeException { private static final long
	 * serialVersionUID = -8260973295443312746L; }
	 */

	/*
	 * public enum SyncResultCode { FAIL_ONE_MODLIST_IS_NULL, FAIL_TWO_MODLIST_IS_NULL,
	 * FAIL_ONE_GET_PAGES, FAIL_TWO_GET_PAGES, FAIL_ONE_POST_PAGES, FAIL_TWO_POST_PAGES, DONE }
	 */

	// /**
	// * Результат выполнения синхронизации
	// * @author Bosik
	// */
	// public class SyncResultDep
	// {
	// /**
	// * Код возврата
	// */
	// //public final SyncResultCode code;
	// /**
	// * Количество страниц, переданных из первого источника во второй
	// */
	// public final int transferedFirstToSecond = 0;
	// /**
	// * Количество страниц, переданных из второго источника в первый
	// */
	// public final int transferedSecondToFirst = 0;
	//
	// /*public SyncResult(SyncResultCode code, int transferedFirstToSecond, int
	// transferedSecondToFirst)
	// {
	// this.code = code;
	// this.transferedFirstToSecond = transferedFirstToSecond;
	// this.transferedSecondToFirst = transferedSecondToFirst;
	// }*/
	// }

	/* ============================ МЕТОДЫ ============================ */

	// TODO: public только для тестирования

	/**
	 * Определяет, отсортирован ли массив по возрастанию дат
	 * 
	 * @param modList
	 *            Массив
	 * @return Отсортированность
	 */
	public static boolean ordered(List<PageVersion> modList)
	{
		for (int i = 0; i < (modList.size() - 1); i++)
		{
			if (modList.get(i).date.compareTo(modList.get(i + 1).date) > 0)
			{
				return false;
			}
		}
		return true;
	}

	/**
	 * Сортирует указанный отрезок массива по возрастанию дат
	 * 
	 * @param modList
	 *            Массив
	 * @param left
	 *            Начало отрезка
	 * @param right
	 *            Конец отрезка
	 */
	public static void sort(List<PageVersion> modList, int left, int right)
	{
		int i = left;
		int j = right;
		Date x = modList.get((left + right) / 2).date;
		PageVersion y;

		do
		{
			while (modList.get(i).date.compareTo(x) < 0)
			{
				i++;
			}
			while (modList.get(j).date.compareTo(x) > 0)
			{
				j--;
			}
			if (i <= j)
			{
				if (modList.get(i).date != modList.get(j).date)
				{
					y = modList.get(i);
					modList.set(i, modList.get(j));
					modList.set(j, y);
				}
				i++;
				j--;
			}
		}
		while (i <= j);

		if (left < j)
		{
			sort(modList, left, j);
		}
		if (i < right)
		{
			sort(modList, i, right);
		}
	}

	/**
	 * Сортирует указанный массив по возрастанию дат
	 * 
	 * @param modList
	 *            Массив
	 */
	public static void sort(List<PageVersion> modList)
	{
		if (!modList.isEmpty() && !ordered(modList))
		{
			sort(modList, 0, modList.size() - 1);
		}
	}

	/**
	 * Получает списки дат, для которых необходимо произвести синхронизацию
	 * 
	 * @param modList1
	 *            Первый массив
	 * @param modList2
	 *            Второй массив
	 * @param over1
	 *            Список дат, которые присутствуют только в первом списке либо в обоих, но в первом
	 *            версия старше
	 * @param over2
	 *            Список дат, которые присутствуют только во втором списке либо в обоих, но во
	 *            втором версия старше
	 */
	public static void getOverLists(List<PageVersion> modList1, List<PageVersion> modList2, List<Date> over1,
			List<Date> over2)
	{
		// проверки
		if (null == modList1)
		{
			throw new IllegalArgumentException("modList1 can't be null");
		}
		if (null == modList2)
		{
			throw new IllegalArgumentException("modList2 can't be null");
		}
		if (null == over1)
		{
			throw new IllegalArgumentException("over1 can't be null");
		}
		if (null == over2)
		{
			throw new IllegalArgumentException("over2 can't be null");
		}

		// подготовка
		sort(modList1);
		sort(modList2);
		over1.clear();
		over2.clear();
		int i = 0;
		int j = 0;

		// параллельная обработка
		while ((i < modList1.size()) && (j < modList2.size()))
		{
			PageVersion p1 = modList1.get(i);
			PageVersion p2 = modList2.get(j);
			int c = p1.date.compareTo(p2.date);
			if (c < 0)
			{
				over1.add(p1.date);
				i++;
			}
			else
				if (c > 0)
				{
					over2.add(p2.date);
					j++;
				}
				else
				{
					if (p1.version > p2.version)
					{
						over1.add(p1.date);
					}
					else
						if (p1.version < p2.version)
						{
							over2.add(p2.date);
						}
					i++;
					j++;
				}
		}

		// добиваем первый
		while (i < modList1.size())
		{
			over1.add(modList1.get(i).date);
			i++;
		}

		// добиваем второй
		while (j < modList2.size())
		{
			over2.add(modList2.get(j).date);
			j++;
		}
	}

	/**
	 * Синхронизирует источники. Учитываются только страницы, изменённые после указанного времени
	 * 
	 * @param source1
	 *            Первый источник
	 * @param source2
	 *            Второй источник
	 * @param since
	 *            Время последней синхронизации
	 * @return Суммарное количество переданных страниц
	 */
	public static int synchronize(DiaryDAO source1, DiaryDAO source2, Date since)
	{
		// проверки
		if (null == source1)
		{
			throw new NullPointerException("source1 can't be null");
		}
		if (null == source2)
		{
			throw new NullPointerException("source2 can't be null");
		}
		if (null == since)
		{
			throw new NullPointerException("since date can't be null");
		}

		// запрашиваем modlist
		List<PageVersion> modList1 = source1.getModList(since);
		List<PageVersion> modList2 = source2.getModList(since);

		// проверяем (TODO: fail-fast)

		if (null == modList1)
		{
			throw new NullPointerException("modList1 is null");
		}
		if (null == modList2)
		{
			throw new NullPointerException("modList2 is null");
		}

		// получаем списки для передачи

		List<Date> over1 = new ArrayList<Date>();
		List<Date> over2 = new ArrayList<Date>();
		getOverLists(modList1, modList2, over1, over2);

		// debug

		/*
		 * if (BuildConfig.DEBUG) { Log.v(TAG, "1 --> 2 : total count: " +
		 * String.valueOf(over1.size())); for (int i = 0; i < over1.size(); i++) { Log.v(TAG,
		 * "1 --> 2 : " + String.valueOf(over1.get(i))); } Log.v(TAG, "2 --> 1 : total count: " +
		 * String.valueOf(over2.size())); for (int i = 0; i < over2.size(); i++) { Log.v(TAG,
		 * "2 --> 1 : " + String.valueOf(over2.get(i))); } }
		 */

		// синхронизация

		// THINK: разбивать на группы?
		source1.postPages(source2.getPages(over2));
		source2.postPages(source1.getPages(over1));

		// результат
		return over1.size() + over2.size();
	}
}
