package org.bosik.compensation.persistence.dao;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.fakes.mocks.MockDiaryPage;
import org.bosik.compensation.fakes.mocks.Mock;
import android.test.AndroidTestCase;

public abstract class TestDiaryDAO extends AndroidTestCase
{
	private DiaryDAO						diaryDAO;
	private static final Mock<DiaryPage>	mockDiaryPage	= new MockDiaryPage();

	protected abstract DiaryDAO getDAO();

	@Override
	protected void setUp()
	{
		diaryDAO = getDAO();
	}

	public void testPersistanceSingle()
	{
		DiaryPage org = mockDiaryPage.getSamples().get(0);
		diaryDAO.postPage(org);

		// ------------------
		setUp();

		DiaryPage restored = diaryDAO.getPage(org.getDate());
		mockDiaryPage.compare(org, restored);
	}

	public void testPersistanceMultiple()
	{
		List<DiaryPage> orgPages = mockDiaryPage.getSamples();
		diaryDAO.postRecords(orgPages);

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
			mockDiaryPage.compare(orgPages.get(i), restoredPages.get(i));
		}
	}

	public void testGetModList()
	{
		// fail("Not yet implemented"); // TODO
	}
}
