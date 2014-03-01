package org.bosik.diacomp.web.backend.features.diary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.fakes.mocks.Mock;
import org.bosik.diacomp.core.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.core.fakes.mocks.VersionedUtils;
import org.bosik.diacomp.core.services.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.frontend.features.auth.AuthRememberService;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.features.diary.DiaryAuthorizedService;
import org.bosik.diacomp.web.frontend.features.diary.DiaryRestClient;
import org.junit.Test;

public class TestDiaryWebService
{
	private DiaryService	diaryService;

	public TestDiaryWebService()
	{
		String login = Config.getLogin();
		String pass = Config.getPassword();
		int apiVersion = Config.getAPICurrent();
		AuthRememberService authService = new AuthRememberService(new AuthRestClient(), login, pass, apiVersion);

		diaryService = new DiaryAuthorizedService(new DiaryRestClient(), authService);
	}

	//	private void login()
	//	{
	//		authService.login(Config.getLogin(), Config.getPassword(), Config.getAPICurrent());
	//	}

	/**
	 * _simple-tests doesn't check any business logic. The only aspect the test is ability to invoke
	 * the method
	 */

	//	@Test(expected = NotAuthorizedException.class)
	//	public void getRecordsViaGuids_Unauth_Exception()
	//	{
	//		authService.logout();
	//		diaryService.getRecords(Collections.<String> emptyList());
	//	}
	//
	//	@Test(expected = NotAuthorizedException.class)
	//	public void getRecordsNew_Unauth_Exception()
	//	{
	//		authService.logout();
	//		diaryService.getRecords(new Date(), true);
	//	}
	//
	//	@Test(expected = NotAuthorizedException.class)
	//	public void getRecordsPeriod_Unauth_Exception()
	//	{
	//		authService.logout();
	//		diaryService.getRecords(new Date(), new Date(), true);
	//	}

	@Test
	public void getRecordsViaGuids_Simple_Ok()
	{
		//authService.login();
		diaryService.getRecords(Arrays.<String> asList("abc", "def"));
	}

	@Test
	public void getRecordsNew_Simple_ok()
	{
		//		authService.login();
		diaryService.getRecords(new Date(), true);
		diaryService.getRecords(new Date(), false);
	}

	@Test
	public void getRecordsPeriod_Simple_ok()
	{
		//		authService.login();
		diaryService.getRecords(new Date(), new Date(), true);
		diaryService.getRecords(new Date(), new Date(), false);
	}

	@Test
	public void getRecordsViaPeriod_Normal_RestoredOrdered()
	{
		Mock<DiaryRecord> mockRecord = new MockDiaryRecord();
		Mock<Versioned<DiaryRecord>> mockVersioned = new MockVersionedConverter<DiaryRecord>(mockRecord);
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerate(originalItems);

		assertTrue("No samples are provided", !originalItems.isEmpty());

		Date minTime = null;
		Date maxTime = null;
		for (Versioned<DiaryRecord> item : originalItems)
		{
			Date time = item.getData().getTime();
			if ((minTime == null) || time.before(minTime))
			{
				minTime = time;
			}

			if ((maxTime == null) || time.after(maxTime))
			{
				maxTime = time;
			}
		}

		// Check if there are any records in that period

		minTime = new Date(minTime.getTime() - (60 * 1000));
		maxTime = new Date(maxTime.getTime() + (60 * 1000));
		List<Versioned<DiaryRecord>> restoredItems = diaryService.getRecords(minTime, maxTime, true);

		if (restoredItems.isEmpty())
		{
			diaryService.postRecords(originalItems);
		}

		// Check the order
		restoredItems = diaryService.getRecords(minTime, maxTime, true);
		checkTimeOrder(restoredItems);
	}

	@Test
	public void postRecordsGetRecordsViaPeriodAndGuid_Normal_RestoredExactly()
	{
		Mock<DiaryRecord> mockRecord = new MockDiaryRecord();
		Mock<Versioned<DiaryRecord>> mockVersioned = new MockVersionedConverter<DiaryRecord>(mockRecord);
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerate(originalItems);

		Date minTime = null;
		Date maxTime = null;
		for (Versioned<DiaryRecord> item : originalItems)
		{
			Date time = item.getData().getTime();
			if ((minTime == null) || time.before(minTime))
			{
				minTime = time;
			}

			if ((maxTime == null) || time.after(maxTime))
			{
				maxTime = time;
			}
		}

		assertNotNull(minTime);
		assertNotNull(maxTime);
		minTime = new Date(minTime.getTime() - (60 * 1000));
		maxTime = new Date(maxTime.getTime() + (60 * 1000));

		// Post

		diaryService.postRecords(originalItems);

		// Check via GUIDs

		for (Versioned<DiaryRecord> item : originalItems)
		{
			List<Versioned<DiaryRecord>> restored = diaryService.getRecords(Arrays.<String> asList(item.getId()));
			assertNotNull(restored);
			assertEquals(1, restored.size());

			mockVersioned.compare(item, restored.get(0));
		}

		// Check via period: 

		List<Versioned<DiaryRecord>> restoredItems = diaryService.getRecords(minTime, maxTime, true);
		assertTrue(!restoredItems.isEmpty());
		checkTimeOrder(restoredItems);

		for (Versioned<DiaryRecord> originalItem : originalItems)
		{
			boolean found = false;
			for (Versioned<DiaryRecord> restoredItem : restoredItems)
			{
				if (restoredItem.getId().equals(originalItem.getId()))
				{
					mockVersioned.compare(originalItem, restoredItem);
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	private static void checkTimeOrder(List<Versioned<DiaryRecord>> items)
	{
		// Check the order
		for (int i = 0; i < (items.size() - 1); i++)
		{
			String timeLess = Utils.formatTimeUTC(items.get(i).getData().getTime());
			String timeMore = Utils.formatTimeUTC(items.get(i + 1).getData().getTime());
			if (timeLess.compareTo(timeMore) > 0)
			{
				// debug print
				System.out.println();
				System.out.println("Items:");
				for (int j = 0; j < (items.size()); j++)
				{
					final String guid = items.get(j).getId();
					final String time = Utils.formatTimeUTC(items.get(j).getData().getTime());
					System.out.println(j + "\t" + guid + " " + time);
				}

				fail(String.format("Items #%d and #%d are wrong ordered", i, i + 1));
			}
		}
	}

	@Test
	public void postRecords_Update_UpdatedOk()
	{
		Mock<DiaryRecord> mockRecord = new MockDiaryRecord();
		Mock<Versioned<DiaryRecord>> mockVersioned = new MockVersionedConverter<DiaryRecord>(mockRecord);
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerate(originalItems);

		// Insertion test

		diaryService.postRecords(originalItems);

		for (Versioned<DiaryRecord> item : originalItems)
		{
			List<Versioned<DiaryRecord>> restored = diaryService.getRecords(Arrays.<String> asList(item.getId()));
			assertNotNull(restored);
			assertEquals(1, restored.size());

			mockVersioned.compare(item, restored.get(0));
		}

		// Updating test

		for (Versioned<DiaryRecord> item : originalItems)
		{
			item.getData().setTime(new Date());
			item.setDeleted(!item.isDeleted());
			item.setTimeStamp(new Date());
			item.setVersion(item.getVersion() + 1);
		}

		diaryService.postRecords(originalItems);

		for (Versioned<DiaryRecord> item : originalItems)
		{
			List<Versioned<DiaryRecord>> restored = diaryService.getRecords(Arrays.<String> asList(item.getId()));
			assertNotNull(restored);
			assertEquals(1, restored.size());

			mockVersioned.compare(item, restored.get(0));
		}
	}
}
