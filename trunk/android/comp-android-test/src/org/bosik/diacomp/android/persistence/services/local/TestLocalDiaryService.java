package org.bosik.diacomp.android.persistence.services.local;

import org.bosik.diacomp.android.backend.features.diary.LocalDiaryService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryServiceCommon;
import org.bosik.diacomp.core.services.diary.TestDiaryService;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class TestLocalDiaryService extends AndroidTestCase implements TestDiaryService
{
	private static TestDiaryServiceCommon	test;

	@Override
	protected void setUp()
	{
		assertNotNull(getContext());
		ContentResolver resolver = getContext().getContentResolver();
		final DiaryService service = new LocalDiaryService(resolver);
		test = new TestDiaryServiceCommon()
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
	// TestDiaryServiceCommon test = new TestDiaryServiceCommon()
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
	// TestDiaryServiceCommon test = new TestDiaryServiceCommon()
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

	@Override
	public void test_PostRecordsGetRecords_Deleting_Removed()
	{
		test.test_PostRecordsGetRecords_Deleting_Removed();
	}

	@Override
	public void test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOrdered()
	{
		test.test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOrdered();
	}

	@Override
	public void test_PostRecordsGetRecordsViaTimestamp_Normal_RestoredOK()
	{
		test.test_PostRecordsGetRecordsViaTimestamp_Normal_RestoredOK();
	}

	@Override
	public void test_PostRecordsGetRecordsViaGuid_Normal_RestoredOK()
	{
		test.test_PostRecordsGetRecordsViaGuid_Normal_RestoredOK();
	}

	@Override
	public void test_PostRecordsGetRecordsViaGuid_Update_UpdatedOk()
	{
		test.test_PostRecordsGetRecordsViaGuid_Update_UpdatedOk();
	}

	@Override
	public void test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOK()
	{
		test.test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOK();
	}
}