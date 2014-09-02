package org.bosik.diacomp.android.backend.features.diary;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class TestDiaryLocalServicePerformance extends AndroidTestCase
{
	public void test_PerfomanceOnSelect()
	{
		assertNotNull(getContext());
		ContentResolver resolver = getContext().getContentResolver();
		final DiaryService service = new DiaryLocalService(resolver);

		long start = System.currentTimeMillis();

		Date fromTime = Utils.date(2014, 01, 01);
		Date toTime = Utils.date(2014, 12, 31);
		List<Versioned<DiaryRecord>> items = service.findBetween(fromTime, toTime, true);

		long time = System.currentTimeMillis() - start;
		if (items.size() > 0)
		{
			if (time > 0)
			{
				double speed = (double) items.size() / time * 1000;
				System.out.println(String.format("%d items parsed in %d msec (%.2f items/sec)", items.size(), time,
						speed));
			}
			else
			{
				System.out.println(String.format("%d items parsed in %d msec (speed in not available)", items.size(),
						time));
			}
		}
		else
		{
			System.out.println("No items found");
		}
	}
}
