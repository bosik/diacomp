package org.bosik.diacomp.web.backend.features.diary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.fakes.mocks.Mock;
import org.bosik.diacomp.core.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.DiaryService;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.features.diary.DiaryRestClient;
import org.junit.Test;

public class TestDiaryWebService
{
	private AuthService		authService		= new AuthRestClient();
	private DiaryService	diaryService	= new DiaryRestClient();

	private void login()
	{
		authService.login(Config.getLogin(), Config.getPassword(), Config.getAPICurrent());
	}

	/**
	 * _simple-tests doesn't check any business logic. The only aspect the test is ability to invoke
	 * the method
	 */

	@Test(expected = NotAuthorizedException.class)
	public void getRecordsViaGuids_Unauth_Exception()
	{
		authService.logout();
		diaryService.getRecords(Collections.<String> emptyList());
	}

	@Test(expected = NotAuthorizedException.class)
	public void getRecordsNew_Unauth_Exception()
	{
		authService.logout();
		diaryService.getRecords(new Date(), true);
	}

	@Test(expected = NotAuthorizedException.class)
	public void getRecordsPeriod_Unauth_Exception()
	{
		authService.logout();
		diaryService.getRecords(new Date(), new Date(), true);
	}

	@Test
	public void getRecordsViaGuids_Simple_Ok()
	{
		login();
		diaryService.getRecords(Arrays.<String> asList("abc", "def"));
	}

	@Test
	public void getRecordsNew_Simple_ok()
	{
		login();
		diaryService.getRecords(new Date(), true);
		diaryService.getRecords(new Date(), false);
	}

	@Test
	public void getRecordsPeriod_Simple_ok()
	{
		login();
		diaryService.getRecords(new Date(), new Date(), true);
		diaryService.getRecords(new Date(), new Date(), false);
	}

	@Test
	public void postRecords_Insert_RestoredOrdered()
	{
		Mock<DiaryRecord> mockRecord = new MockDiaryRecord();
		Mock<Versioned<DiaryRecord>> mockVersioned = new MockVersionedConverter<DiaryRecord>(mockRecord);
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();

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

		login();

		// Check if there are no records in that period yet

		minTime = new Date(minTime.getTime() - (60 * 1000));
		maxTime = new Date(maxTime.getTime() + (60 * 1000));
		List<Versioned<DiaryRecord>> restoredItems = diaryService.getRecords(minTime, maxTime, true);
		assertTrue(restoredItems.isEmpty());

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

		// Check via period

		restoredItems = diaryService.getRecords(minTime, maxTime, true);
		assertTrue(!restoredItems.isEmpty());
		assertEquals(originalItems.size(), restoredItems.size());

		// Check the order
		for (int i = 0; i < (restoredItems.size() - 1); i++)
		{
			Date timeLess = restoredItems.get(i).getData().getTime();
			Date timeMore = restoredItems.get(i + 1).getData().getTime();
			assertTrue(!timeLess.after(timeMore));
		}

		// Check the content
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

	@Test
	public void postRecords_Update_UpdatedOk()
	{
		Mock<DiaryRecord> mockRecord = new MockDiaryRecord();
		Mock<Versioned<DiaryRecord>> mockVersioned = new MockVersionedConverter<DiaryRecord>(mockRecord);
		List<Versioned<DiaryRecord>> items = mockVersioned.getSamples();

		login();

		// Insertion test

		diaryService.postRecords(items);
		for (Versioned<DiaryRecord> item : items)
		{
			List<Versioned<DiaryRecord>> restored = diaryService.getRecords(Arrays.<String> asList(item.getId()));
			assertNotNull(restored);
			assertEquals(1, restored.size());

			mockVersioned.compare(item, restored.get(0));
		}

		// Updating test

		for (Versioned<DiaryRecord> item : items)
		{
			item.getData().setTime(new Date());
			item.setDeleted(item.isDeleted());
			item.setTimeStamp(new Date());
			item.setVersion(item.getVersion() + 1);
		}

		diaryService.postRecords(items);

		for (Versioned<DiaryRecord> item : items)
		{
			List<Versioned<DiaryRecord>> restored = diaryService.getRecords(Arrays.<String> asList(item.getId()));
			assertNotNull(restored);
			assertEquals(1, restored.size());

			mockVersioned.compare(item, restored.get(0));
		}
	}
}
