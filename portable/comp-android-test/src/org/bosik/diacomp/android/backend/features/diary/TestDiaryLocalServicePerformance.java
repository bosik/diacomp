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

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import android.test.AndroidTestCase;

public class TestDiaryLocalServicePerformance extends AndroidTestCase
{
	public void test_PerfomanceOnSelect()
	{
		assertNotNull(getContext());
		final DiaryService service = new DiaryLocalService(getContext());

		long start = System.currentTimeMillis();

		Date startTime = Utils.date(2014, 01, 01);
		Date endTime = Utils.date(2014, 12, 31);
		List<Versioned<DiaryRecord>> items = service.findPeriod(startTime, endTime, true);

		long time = System.currentTimeMillis() - start;
		if (items.size() > 0)
		{
			if (time > 0)
			{
				double speed = (double) items.size() / time * 1000;
				System.out.println(
						String.format("%d items parsed in %d msec (%.2f items/sec)", items.size(), time, speed));
			}
			else
			{
				System.out.println(
						String.format("%d items parsed in %d msec (speed in not available)", items.size(), time));
			}
		}
		else
		{
			System.out.println("No items found");
		}
	}
}
