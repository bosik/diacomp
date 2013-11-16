package org.bosik.compensation.persistence.sync;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.compensation.persistence.repository.diary.DiaryRepository.PageVersion;

public class SyncDiaryTest extends TestCase
{

	public void testOrdered_0()
	{
		// пустой массив
		List<PageVersion> modList = new ArrayList<PageVersion>();
		assertTrue(SyncDiaryRepository.ordered(modList));
	}

	public void testOrdered_1()
	{
		// массив из одного элемента
		List<PageVersion> modList = new ArrayList<PageVersion>();
		modList.add(new PageVersion(new Date(2013, 01, 01), 23));
		assertTrue(SyncDiaryRepository.ordered(modList));
	}

	public void testOrdered_3_ordered()
	{
		// массив из трёх упорядоченных элементов
		List<PageVersion> modList = new ArrayList<PageVersion>();
		modList.add(new PageVersion(new Date(2013, 01, 01), 23));
		modList.add(new PageVersion(new Date(2013, 01, 02), 11));
		modList.add(new PageVersion(new Date(2013, 01, 03), 48));
		assertTrue(SyncDiaryRepository.ordered(modList));
	}

	public void testOrdered_3_unordered_1()
	{
		// массив из трёх неупорядоченных элементов
		List<PageVersion> modList = new ArrayList<PageVersion>();
		modList.add(new PageVersion(new Date(2013, 01, 03), 23));
		modList.add(new PageVersion(new Date(2013, 01, 02), 11));
		modList.add(new PageVersion(new Date(2013, 01, 01), 48));
		assertFalse(SyncDiaryRepository.ordered(modList));
	}

	public void testOrdered_3_unordered_2()
	{
		// массив из трёх неупорядоченных элементов
		List<PageVersion> modList = new ArrayList<PageVersion>();
		modList.add(new PageVersion(new Date(2013, 01, 03), 23));
		modList.add(new PageVersion(new Date(2013, 01, 04), 11));
		modList.add(new PageVersion(new Date(2013, 01, 01), 48));
		assertFalse(SyncDiaryRepository.ordered(modList));
	}

	public void testSort_0()
	{
		// пустой массив
		List<PageVersion> modList = new ArrayList<PageVersion>();
		SyncDiaryRepository.sort(modList);
		assertTrue(SyncDiaryRepository.ordered(modList));
	}

	public void testSort_1()
	{
		// массив из одного элемента
		List<PageVersion> modList = new ArrayList<PageVersion>();
		modList.add(new PageVersion(new Date(2013, 01, 03), 23));
		SyncDiaryRepository.sort(modList);
		assertTrue(SyncDiaryRepository.ordered(modList));
	}

	public void testSort_3_unordered()
	{
		// массив из трёх неупорядоченных элементов
		List<PageVersion> modList = new ArrayList<PageVersion>();
		modList.add(new PageVersion(new Date(2013, 01, 03), 23));
		modList.add(new PageVersion(new Date(2013, 01, 02), 11));
		modList.add(new PageVersion(new Date(2013, 01, 01), 48));
		SyncDiaryRepository.sort(modList);
		assertTrue(SyncDiaryRepository.ordered(modList));
	}

	public void testSort_3_ordered()
	{
		// массив из трёх упорядоченных элементов
		List<PageVersion> modList = new ArrayList<PageVersion>();
		modList.add(new PageVersion(new Date(2013, 01, 01), 23));
		modList.add(new PageVersion(new Date(2013, 01, 02), 11));
		modList.add(new PageVersion(new Date(2013, 01, 06), 48));
		SyncDiaryRepository.sort(modList);
		assertTrue(SyncDiaryRepository.ordered(modList));
	}

	public void testGetOverLists1()
	{
		List<PageVersion> modList1 = new ArrayList<PageVersion>();
		modList1.add(new PageVersion(new Date(2013, 01, 01), 23));
		modList1.add(new PageVersion(new Date(2013, 01, 06), 48));
		modList1.add(new PageVersion(new Date(2013, 01, 02), 11));

		List<PageVersion> modList2 = new ArrayList<PageVersion>();
		modList2.add(new PageVersion(new Date(2013, 01, 01), 23));
		modList2.add(new PageVersion(new Date(2013, 01, 02), 11));
		modList2.add(new PageVersion(new Date(2013, 01, 06), 48));

		List<Date> over1 = new ArrayList<Date>();
		List<Date> over2 = new ArrayList<Date>();

		SyncDiaryRepository.getOverLists(modList1, modList2, over1, over2);

		assertTrue(over1.isEmpty());
		assertTrue(over2.isEmpty());
	}

	// TODO: тестировать остальные ситуации getOverLists
}
