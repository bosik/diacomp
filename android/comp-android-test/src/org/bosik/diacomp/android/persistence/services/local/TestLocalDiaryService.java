package org.bosik.diacomp.android.persistence.services.local;

import org.bosik.diacomp.android.backend.features.diary.LocalDiaryService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryService;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class TestLocalDiaryService extends AndroidTestCase
{
	private static TestDiaryService	test;

	@Override
	protected void setUp()
	{
		assertNotNull(getContext());
		ContentResolver resolver = getContext().getContentResolver();
		final DiaryService service = new LocalDiaryService(resolver);
		test = new TestDiaryService()
		{
			@Override
			protected DiaryService getService()
			{
				return service;
			}
		};
		test.setUp();
	}

	// @Override
	// public TestResult run()
	// {
	// assertNotNull(getContext());
	// ContentResolver resolver = getContext().getContentResolver();
	// final DiaryService service = new LocalDiaryService(resolver);
	//
	// TestDiaryService test = new TestDiaryService()
	// {
	// @Override
	// protected DiaryService getService()
	// {
	// return service;
	// }
	// };
	// return test.run();
	// }

	// private static final String TAG = TestLocalDiaryService.class.getSimpleName();

	// public void testAll() throws Throwable
	// {
	// assertNotNull(getContext());
	// ContentResolver resolver = getContext().getContentResolver();
	// final DiaryService service = new LocalDiaryService(resolver);
	//
	// TestDiaryService test = new TestDiaryService()
	// {
	// @Override
	// protected DiaryService getService()
	// {
	// return service;
	// }
	// };
	//
	// test.setName("test_postRecordsGetRecordsViaPeriodAndGuid_Normal_RestoredExactly");
	// Log.e(TAG, "countTestCases: " + String.valueOf(test.countTestCases()));
	// Log.e(TAG, "testCount: " + String.valueOf(test.testCount()));
	// }

	public void test_getRecords_Deleting_Removed()
	{
		test.test_getRecords_Deleting_Removed();
	}

	public void test_getRecordsViaPeriod_Normal_RestoredOrdered()
	{
		test.test_getRecordsViaPeriod_Normal_RestoredOrdered();
	}

	public void test_getRecordsViaTimestamp_Normal_ok()
	{
		test.test_getRecordsViaTimestamp_Normal_ok();
	}

	public void test_PersistanceMultiple()
	{
		test.test_PersistanceMultiple();
	}

	public void test_postRecords_Update_UpdatedOk()
	{
		test.test_postRecords_Update_UpdatedOk();
	}

	public void test_postRecordsGetRecordsViaPeriodAndGuid_Normal_RestoredExactly()
	{
		test.test_postRecordsGetRecordsViaPeriodAndGuid_Normal_RestoredExactly();
	}
}