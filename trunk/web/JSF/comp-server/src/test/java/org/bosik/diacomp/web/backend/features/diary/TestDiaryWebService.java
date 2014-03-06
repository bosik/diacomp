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
import org.bosik.diacomp.core.services.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.core.utils.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.core.utils.test.fakes.mocks.VersionedUtils;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.frontend.features.auth.AuthRememberService;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.features.diary.DiaryAuthorizedService;
import org.bosik.diacomp.web.frontend.features.diary.DiaryRestClient;
import org.junit.Test;

public class TestDiaryWebService
{
	private DiaryService					diaryService;

	private Mock<DiaryRecord>				mockRecord		= new MockDiaryRecord();
	private Mock<Versioned<DiaryRecord>>	mockVersioned	= new MockVersionedConverter<DiaryRecord>(mockRecord);

	private static final long				DELTA			= 60000;

	public TestDiaryWebService()
	{
		String login = Config.getLogin();
		String pass = Config.getPassword();
		int apiVersion = Config.getAPICurrent();
		AuthRememberService authService = new AuthRememberService(new AuthRestClient(), login, pass, apiVersion);

		diaryService = new DiaryAuthorizedService(new DiaryRestClient(), authService);
	}

	/**
	 * Checks if all exp items are presented in the act list
	 * 
	 * @param exp
	 * @param act
	 * @param allowExtraItems
	 *            Is it allowed act list to has extra items, which are not presented in the exp list
	 */
	private void compareItems(List<Versioned<DiaryRecord>> exp, List<Versioned<DiaryRecord>> act,
			boolean allowExtraItems)
	{
		if (!allowExtraItems)
		{
			assertEquals(exp.size(), act.size());
		}

		for (Versioned<DiaryRecord> expectedItem : exp)
		{
			boolean found = false;
			for (Versioned<DiaryRecord> actualItem : act)
			{
				if (actualItem.getId().equals(expectedItem.getId()))
				{
					mockVersioned.compare(expectedItem, actualItem);
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	/**
	 * Checks if items are sorted by time ascendingly
	 * 
	 * @param items
	 */
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
	public void getRecordsViaTimestamp_Normal_ok()
	{
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerate(originalItems);
		assertTrue("No samples are provided", !originalItems.isEmpty());

		Date timeBefore = Utils.time(2020, 01, 15, 10, 00, 00);
		Date timeLine = Utils.time(2020, 01, 15, 12, 00, 00);
		Date timeAfter = Utils.time(2020, 01, 15, 14, 00, 00);

		// setup timestamp before line
		for (Versioned<DiaryRecord> item : originalItems)
		{
			item.setTimeStamp(timeBefore);
		}
		diaryService.postRecords(originalItems);

		// check if there are no items after line
		List<Versioned<DiaryRecord>> restoredItems = diaryService.getRecords(timeLine, true);
		assertTrue("Clear base before testing", restoredItems.isEmpty());

		// modify timestamp
		for (Versioned<DiaryRecord> item : originalItems)
		{
			item.setTimeStamp(timeAfter);
		}
		diaryService.postRecords(originalItems);

		// check the result
		restoredItems = diaryService.getRecords(timeLine, true);
		compareItems(originalItems, restoredItems, false);
	}

	@Test
	public void getRecords_Deleting_Removed()
	{
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerate(originalItems);
		assertTrue("No samples are provided", !originalItems.isEmpty());

		// prepare single non-deleted item
		Versioned<DiaryRecord> item = originalItems.get(0);
		item.setDeleted(false);
		originalItems.clear();
		originalItems.add(item);

		final Date timestampBefore = new Date(item.getTimeStamp().getTime() - DELTA);
		final Date timeBefore = new Date(item.getData().getTime().getTime() - DELTA);
		final Date timeAfter = new Date(item.getData().getTime().getTime() + DELTA);

		// post it
		diaryService.postRecords(originalItems);

		// CHECK IF IT IS PRESENTED NOW:

		// 1. Via GUID
		List<Versioned<DiaryRecord>> restoredItems = diaryService.getRecords(Arrays.asList(item.getId()));
		compareItems(originalItems, restoredItems, false);

		// 2. Via timestamp
		restoredItems = diaryService.getRecords(timestampBefore, false);
		compareItems(originalItems, restoredItems, true);

		// 3. Via period
		restoredItems = diaryService.getRecords(timeBefore, timeAfter, false);
		compareItems(originalItems, restoredItems, true);

		// REMOVE IT
		item.setDeleted(true);
		diaryService.postRecords(originalItems);

		// CHECK IF IT IS [MARKED AS] REMOVED

		// 1. Via GUID
		restoredItems = diaryService.getRecords(Arrays.asList(item.getId()));
		compareItems(originalItems, restoredItems, false);

		// 2. Via timestamp
		restoredItems = diaryService.getRecords(timestampBefore, false);
		for (Versioned<DiaryRecord> restoredItem : restoredItems)
		{
			if (restoredItem.getId().equals(item.getId()))
			{
				fail("Item is not deleted");
			}
		}

		// 3. Via period
		restoredItems = diaryService.getRecords(timeBefore, timeAfter, false);
		for (Versioned<DiaryRecord> restoredItem : restoredItems)
		{
			if (restoredItem.getId().equals(item.getId()))
			{
				fail("Item is not deleted");
			}
		}

		// RESTORE IT
		item.setDeleted(false);
		diaryService.postRecords(originalItems);

		// CHECK IF IT IS PRESENTED AGAIN:

		// 1. Via GUID
		restoredItems = diaryService.getRecords(Arrays.asList(item.getId()));
		compareItems(originalItems, restoredItems, false);

		// 2. Via timestamp
		restoredItems = diaryService.getRecords(timestampBefore, false);
		compareItems(originalItems, restoredItems, true);

		// 3. Via period
		restoredItems = diaryService.getRecords(timeBefore, timeAfter, false);
		compareItems(originalItems, restoredItems, true);
	}

	@Test
	public void getRecordsViaPeriod_Normal_RestoredOrdered()
	{
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

		minTime = new Date(minTime.getTime() - DELTA);
		maxTime = new Date(maxTime.getTime() + DELTA);
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
		minTime = new Date(minTime.getTime() - DELTA);
		maxTime = new Date(maxTime.getTime() + DELTA);

		// Post

		diaryService.postRecords(originalItems);

		// Check via GUIDs (one by one)

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
		compareItems(originalItems, restoredItems, true);
	}

	@Test
	public void postRecords_Update_UpdatedOk()
	{
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
