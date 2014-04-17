package org.bosik.diacomp.core.test.fakes.services;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;

public class FakeDiaryService implements DiaryService
{
	private final List<Versioned<DiaryRecord>>	data	= new ArrayList<Versioned<DiaryRecord>>();

	private static void sort(List<Versioned<DiaryRecord>> items)
	{
		Collections.sort(items, new Comparator<Versioned<DiaryRecord>>()
		{
			@Override
			public int compare(Versioned<DiaryRecord> o1, Versioned<DiaryRecord> o2)
			{
				return o1.getData().getTime().compareTo(o2.getData().getTime());
			}
		});
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		for (Versioned<DiaryRecord> item : data)
		{
			if (item.getId().equals(id))
			{
				if (item.isDeleted())
				{
					throw new AlreadyDeletedException(id);
				}
				item.setDeleted(true);
			}
		}
		throw new NotFoundException(id);
	}

	@Override
	public Versioned<DiaryRecord> findById(String id) throws CommonServiceException
	{
		for (Versioned<DiaryRecord> item : data)
		{
			if (item.getId().equals(id))
			{
				return new Versioned<DiaryRecord>(item);
			}
		}
		return null;
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date since) throws CommonServiceException
	{
		List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();

		for (Versioned<DiaryRecord> item : data)
		{
			if (item.getTimeStamp().after(since))
			{
				result.add(new Versioned<DiaryRecord>(item));
			}
		}

		sort(result);
		return result;
	}

	@Override
	public void save(List<Versioned<DiaryRecord>> items) throws CommonServiceException
	{
		for (Versioned<DiaryRecord> item : items)
		{
			boolean found = false;

			for (Versioned<DiaryRecord> x : data)
			{
				if (x.getId().equals(item.getId()))
				{
					x.setTimeStamp(item.getTimeStamp());
					x.setVersion(item.getVersion());
					x.setDeleted(item.isDeleted());
					x.setData(item.getData()); // FIXME: may be problem

					found = true;
					break;
				}
			}

			if (!found)
			{
				data.add(new Versioned<DiaryRecord>(item));
			}
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> findBetween(Date fromTime, Date toTime, boolean includeRemoved)
			throws CommonServiceException
	{
		List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();

		for (Versioned<DiaryRecord> item : data)
		{
			Date time = item.getData().getTime();

			if ((includeRemoved || !item.isDeleted()) && (time.after(fromTime) && time.before(toTime)))
			{
				result.add(new Versioned<DiaryRecord>(item));
			}
		}

		sort(result);
		return result;
	}
}
