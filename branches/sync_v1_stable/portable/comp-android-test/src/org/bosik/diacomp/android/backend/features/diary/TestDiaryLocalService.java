package org.bosik.diacomp.android.backend.features.diary;

import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryServiceCommon;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class TestDiaryLocalService extends AndroidTestCase implements TestDiaryService
{
	private static TestDiaryServiceCommon	test;

	@Override
	protected void setUp() throws Exception
	{
		super.setUp();
		assertNotNull(getContext());
		ContentResolver resolver = getContext().getContentResolver();
		final DiaryService service = new DiaryLocalService(resolver);
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