package org.bosik.diacomp.core.services.diary;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.test.VersionedUtils;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.core.utils.Utils;

public abstract class TestDiaryServiceCommon extends TestCase implements TestDiaryService
{
	private DiaryService								diaryService;
	private static final Mock<DiaryRecord>				mockDiaryRecord	= new MockDiaryRecord();
	private static final Mock<Versioned<DiaryRecord>>	mockVersioned	= new MockVersionedConverter<DiaryRecord>(
																				mockDiaryRecord);

	private static final long							DELTA			= 60000;

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

	/**
	 * Checks if all exp items are presented in the act list
	 *
	 * @param exp
	 * @param act
	 * @param allowExtraItems
	 *            Is it allowed act list to has extra items, which are not presented in the exp list
	 */
	private static void compareItems(List<Versioned<DiaryRecord>> exp, List<Versioned<DiaryRecord>> act,
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

	protected abstract DiaryService getService();

	@Override
	public void setUp()
	{
		diaryService = getService();
		if (diaryService == null)
		{
			fail("Diary service can't be null");
		}
	}

	@Override
	public void test_PostRecordsGetRecords_Deleting_Removed()
	{
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerateGuids(originalItems);
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
		diaryService.save(originalItems);

		// CHECK IF IT IS PRESENTED NOW:

		// 1. Via GUID
		List<Versioned<DiaryRecord>> restoredItems = Arrays.asList(diaryService.findById(item.getId()));
		compareItems(originalItems, restoredItems, false);

		// 2. Via timestamp
		restoredItems = diaryService.findChanged(timestampBefore);
		compareItems(originalItems, restoredItems, true);

		// 3. Via period
		restoredItems = diaryService.findBetween(timeBefore, timeAfter, false);
		compareItems(originalItems, restoredItems, true);

		// REMOVE IT
		item.setDeleted(true);
		diaryService.save(originalItems);

		// CHECK IF IT IS [MARKED AS] REMOVED

		// 1. Via GUID
		restoredItems = Arrays.asList(diaryService.findById(item.getId()));
		compareItems(originalItems, restoredItems, false);

		// 2. Via timestamp
		restoredItems = diaryService.findChanged(timestampBefore);
		for (Versioned<DiaryRecord> restoredItem : restoredItems)
		{
			if (restoredItem.getId().equals(item.getId()) && (!restoredItem.isDeleted()))
			{
				fail("Item is not deleted");
			}
		}

		// 3. Via period
		restoredItems = diaryService.findBetween(timeBefore, timeAfter, false);
		for (Versioned<DiaryRecord> restoredItem : restoredItems)
		{
			if (restoredItem.getId().equals(item.getId()))
			{
				fail("Item is not deleted");
			}
		}

		// RESTORE IT
		item.setDeleted(false);
		diaryService.save(originalItems);

		// CHECK IF IT IS PRESENTED AGAIN:

		// 1. Via GUID
		restoredItems = Arrays.asList(diaryService.findById(item.getId()));
		compareItems(originalItems, restoredItems, false);

		// 2. Via timestamp
		restoredItems = diaryService.findChanged(timestampBefore);
		compareItems(originalItems, restoredItems, true);

		// 3. Via period
		restoredItems = diaryService.findBetween(timeBefore, timeAfter, false);
		compareItems(originalItems, restoredItems, true);
	}

	@Override
	public void test_PostRecordsGetRecordsViaGuid_Normal_RestoredOK()
	{
		List<Versioned<DiaryRecord>> orgs = mockVersioned.getSamples();
		VersionedUtils.enumerateGuids(orgs);
		diaryService.save(orgs);

		// ------------------
		setUp();

		for (Versioned<DiaryRecord> originalRecord : orgs)
		{
			Versioned<DiaryRecord> restoredRecord = diaryService.findById(originalRecord.getId());
			assertNotNull("DiaryService returned null", restoredRecord);
			mockVersioned.compare(originalRecord, restoredRecord);
		}
	}

	@Override
	public void test_PostRecordsGetRecordsViaGuid_Update_UpdatedOk()
	{
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerateGuids(originalItems);

		// Insertion test

		diaryService.save(originalItems);

		for (Versioned<DiaryRecord> item : originalItems)
		{
			Versioned<DiaryRecord> restored = diaryService.findById(item.getId());
			assertNotNull(restored);
			mockVersioned.compare(item, restored);
		}

		// Updating test

		for (Versioned<DiaryRecord> item : originalItems)
		{
			item.getData().setTime(new Date());
			item.setDeleted(!item.isDeleted());
			item.setTimeStamp(new Date());
			item.setVersion(item.getVersion() + 1);
		}

		diaryService.save(originalItems);

		for (Versioned<DiaryRecord> item : originalItems)
		{
			Versioned<DiaryRecord> restored = diaryService.findById(item.getId());
			assertNotNull(restored);
			mockVersioned.compare(item, restored);
		}
	}

	@Override
	public void test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOK()
	{
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerateGuids(originalItems);

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

		diaryService.save(originalItems);

		// Check via period:

		List<Versioned<DiaryRecord>> restoredItems = diaryService.findBetween(minTime, maxTime, true);
		assertTrue(!restoredItems.isEmpty());
		checkTimeOrder(restoredItems);
		compareItems(originalItems, restoredItems, true);
	}

	@Override
	public void test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOrdered()
	{
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerateGuids(originalItems);
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
		List<Versioned<DiaryRecord>> restoredItems = diaryService.findBetween(minTime, maxTime, true);

		if (restoredItems.isEmpty())
		{
			diaryService.save(originalItems);
		}

		// Check the order
		restoredItems = diaryService.findBetween(minTime, maxTime, true);
		checkTimeOrder(restoredItems);
	}

	@Override
	public void test_PostRecordsGetRecordsViaTimestamp_Normal_RestoredOK()
	{
		List<Versioned<DiaryRecord>> originalItems = mockVersioned.getSamples();
		VersionedUtils.enumerateGuids(originalItems);
		assertTrue("No samples are provided", !originalItems.isEmpty());

		Date timeBefore = Utils.time(2020, 01, 15, 10, 00, 00);
		Date timeLine = Utils.time(2020, 01, 15, 12, 00, 00);
		Date timeAfter = Utils.time(2020, 01, 15, 14, 00, 00);

		// setup timestamp before line
		for (Versioned<DiaryRecord> item : originalItems)
		{
			item.setTimeStamp(timeBefore);
		}
		diaryService.save(originalItems);

		// check if there are no items after line
		List<Versioned<DiaryRecord>> restoredItems = diaryService.findChanged(timeLine);
		assertTrue("Clear base before testing", restoredItems.isEmpty());

		// modify timestamp
		for (Versioned<DiaryRecord> item : originalItems)
		{
			item.setTimeStamp(timeAfter);
		}
		diaryService.save(originalItems);

		// check the result
		restoredItems = diaryService.findChanged(timeLine);
		compareItems(originalItems, restoredItems, false);
	}
}
