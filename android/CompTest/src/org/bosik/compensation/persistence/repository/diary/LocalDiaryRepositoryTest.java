package org.bosik.compensation.persistence.repository.diary;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.diary.DiaryPage;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class LocalDiaryRepositoryTest extends AndroidTestCase
{
	private DiaryRepository	repository;

	@Override
	protected void setUp() throws Exception
	{
		ContentResolver resolver = getContext().getContentResolver();
		repository = new LocalDiaryRepository(resolver);
	}

	public void testPersistanceSingle()
	{
		DiaryPage org = DiaryPageUtils.demoPageA();
		repository.postPage(org);
		// ------------------
		DiaryPage restored = repository.getPage(org.getDate());
		DiaryPageUtils.comparePages(org, restored);
	}

	public void testPersistanceMultiple()
	{
		DiaryPage orgA = DiaryPageUtils.demoPageA();
		DiaryPage orgB = DiaryPageUtils.demoPageB();
		List<DiaryPage> pages = new ArrayList<DiaryPage>();
		pages.add(orgA);
		pages.add(orgB);
		repository.postPages(pages);

		List<Date> dates = new ArrayList<Date>();
		for (DiaryPage page : pages)
		{
			dates.add(page.getDate());
		}
		// ------------------
		List<DiaryPage> restored = repository.getPages(dates);
		assertEquals(pages.size(), restored.size());
		for (int i = 0; i < pages.size(); i++)
		{
			DiaryPageUtils.comparePages(pages.get(i), restored.get(i));
		}
	}

	public void testGetModList()
	{
		// fail("Not yet implemented"); // TODO
	}
}
