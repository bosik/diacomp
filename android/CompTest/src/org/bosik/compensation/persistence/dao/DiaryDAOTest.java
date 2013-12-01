package org.bosik.compensation.persistence.dao;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.diary.DiaryPage;
import android.test.AndroidTestCase;

public abstract class DiaryDAOTest extends AndroidTestCase
{
	private DiaryDAO	diaryDAO;

	protected abstract DiaryDAO getDAO();

	@Override
	protected void setUp()
	{
		diaryDAO = getDAO();
	}

	public void testPersistanceSingle()
	{
		DiaryPage org = DiaryPageUtils.demoPageA();
		diaryDAO.postPage(org);

		// ------------------
		setUp();

		DiaryPage restored = diaryDAO.getPage(org.getDate());
		DiaryPageUtils.comparePages(org, restored);
	}

	public void testPersistanceMultiple()
	{
		DiaryPage orgA = DiaryPageUtils.demoPageA();
		DiaryPage orgB = DiaryPageUtils.demoPageB();
		List<DiaryPage> orgPages = new ArrayList<DiaryPage>();
		orgPages.add(orgA);
		orgPages.add(orgB);
		diaryDAO.postPages(orgPages);

		List<Date> dates = new ArrayList<Date>();
		for (DiaryPage page : orgPages)
		{
			dates.add(page.getDate());
		}

		// ------------------
		setUp();

		List<DiaryPage> restoredPages = diaryDAO.getPages(dates);
		assertEquals(orgPages.size(), restoredPages.size());
		for (int i = 0; i < orgPages.size(); i++)
		{
			DiaryPageUtils.comparePages(orgPages.get(i), restoredPages.get(i));
		}
	}

	public void testGetModList()
	{
		// fail("Not yet implemented"); // TODO
	}
}
