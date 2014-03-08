package org.bosik.diacomp.core.services.diary;

public interface TestDiaryServiceContract
{
	void test_PersistanceMultiple();

	void test_getRecordsViaTimestamp_Normal_ok();

	void test_getRecords_Deleting_Removed();

	void test_getRecordsViaPeriod_Normal_RestoredOrdered();

	void test_postRecordsGetRecordsViaPeriodAndGuid_Normal_RestoredExactly();

	void test_postRecords_Update_UpdatedOk();
}
