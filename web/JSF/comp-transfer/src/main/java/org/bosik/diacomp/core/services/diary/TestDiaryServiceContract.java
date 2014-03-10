package org.bosik.diacomp.core.services.diary;

public interface TestDiaryServiceContract
{
	void test_PostRecordsGetRecordsViaGuid_Normal_RestoredOK();

	void test_PostRecordsGetRecordsViaTimestamp_Normal_RestoredOK();

	void test_PostRecordsGetRecords_Deleting_Removed();

	void test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOrdered();

	void test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOK();

	void test_PostRecordsGetRecordsViaGuid_Update_UpdatedOk();
}
