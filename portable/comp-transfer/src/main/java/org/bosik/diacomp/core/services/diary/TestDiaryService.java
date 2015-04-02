/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.services.diary;

public interface TestDiaryService
{
	void test_PostRecordsGetRecordsViaGuid_Normal_RestoredOK();

	void test_PostRecordsGetRecordsViaTimestamp_Normal_RestoredOK();

	void test_PostRecordsGetRecords_Deleting_Removed();

	void test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOrdered();

	void test_PostRecordsGetRecordsViaPeriod_Normal_RestoredOK();

	void test_PostRecordsGetRecordsViaGuid_Update_UpdatedOk();
}
