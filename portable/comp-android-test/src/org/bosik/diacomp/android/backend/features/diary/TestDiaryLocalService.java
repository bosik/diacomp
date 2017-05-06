/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.backend.features.diary;

import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryServiceCommon;
import org.junit.Ignore;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

@Ignore
public class TestDiaryLocalService extends AndroidTestCase implements TestDiaryService
{
	private static TestDiaryServiceCommon test;

	@Override
	protected void setUp() throws Exception
	{
		super.setUp();
		assertNotNull(getContext());
		ContentResolver resolver = getContext().getContentResolver();
		final DiaryService service = new DiaryLocalService(getContext());
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