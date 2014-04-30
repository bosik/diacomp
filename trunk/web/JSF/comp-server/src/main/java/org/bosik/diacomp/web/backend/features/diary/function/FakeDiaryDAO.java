package org.bosik.diacomp.web.backend.features.diary.function;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;

public class FakeDiaryDAO implements DiaryDAO
{
	private static Mock<Versioned<DiaryRecord>>	mock	= new MockVersionedConverter<DiaryRecord>(new MockDiaryRecord());
	private static List<Versioned<DiaryRecord>>	samples	= mock.getSamples();

	private static void sort(List<Versioned<DiaryRecord>> items)
	{
		Collections.sort(items, new Comparator<Versioned<DiaryRecord>>()
		{
			@Override
			public int compare(Versioned<DiaryRecord> o1, Versioned<DiaryRecord> o2)
			{
				Date t1 = o1.getData().getTime();
				Date t2 = o2.getData().getTime();
				return t1.compareTo(t2);
			}
		});
	}

	/**
	 * NOTE: ignores userId
	 */
	@Override
	public List<Versioned<DiaryRecord>> findChanged(int userId, Date time)
	{
		List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();

		for (Versioned<DiaryRecord> rec : samples)
		{
			if (rec.getTimeStamp().after(time))
			{
				Versioned<DiaryRecord> item = new Versioned<DiaryRecord>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(rec.getData()); // FIXME: unlink data
				result.add(item);
			}
		}

		sort(result);
		return result;
	}

	/**
	 * NOTE: ignores userId
	 */
	@Override
	public List<Versioned<DiaryRecord>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved)
	{
		List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();

		for (Versioned<DiaryRecord> rec : samples)
		{
			final DiaryRecord data = rec.getData();
			if (data.getTime().after(startTime) && data.getTime().before(endTime)
					&& (includeRemoved || !rec.isDeleted()))
			{
				Versioned<DiaryRecord> item = new Versioned<DiaryRecord>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(data); // FIXME: unlink data
				result.add(item);
			}
		}

		sort(result);
		return result;
	}

	/**
	 * NOTE: ignores userId
	 */
	@Override
	public void post(int userId, List<Versioned<DiaryRecord>> records)
	{
		for (Versioned<DiaryRecord> item : records)
		{
			boolean found = false;
			for (int i = 0; i < samples.size(); i++)
			{
				if (samples.get(i).equals(item))
				{
					samples.set(i, new Versioned<DiaryRecord>(item));
					found = true;
					break;
				}
			}

			if (!found)
			{
				samples.add(new Versioned<DiaryRecord>(item));
			}
		}
	}

	/**
	 * NOTE: ignores userId
	 */
	@Override
	public Versioned<DiaryRecord> findByGuid(int userId, String guid)
	{
		for (Versioned<DiaryRecord> rec : samples)
		{
			final DiaryRecord data = rec.getData();
			if (rec.getId().equals(guid))
			{
				Versioned<DiaryRecord> item = new Versioned<DiaryRecord>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(data); // FIXME: unlink data
				return item;
			}
		}

		return null;
	}

	@Override
	public void delete(int userId, String id)
	{
		for (Versioned<DiaryRecord> rec : samples)
		{
			final DiaryRecord data = rec.getData();
			if (rec.getId().equals(id))
			{
				rec.setDeleted(true);
				return;
			}
		}
	}
}
