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
package org.bosik.diacomp.core.test.fakes.services;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;

public class FakeDiaryService implements DiaryService
{
	private final Mock<Versioned<DiaryRecord>>	mock			= new MockVersionedConverter<DiaryRecord>(
			new MockDiaryRecord());
	private final List<Versioned<DiaryRecord>>	samples			= new ArrayList<Versioned<DiaryRecord>>();

	private static final int					MAX_READ_ITEMS	= 500;

	public FakeDiaryService(boolean withSampleData)
	{
		if (withSampleData)
		{
			samples.addAll(mock.getSamples());
		}
	}

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

	@Override
	public int count(String prefix)
	{
		int count = 0;

		for (Versioned<DiaryRecord> item : samples)
		{
			if (item.getId().startsWith(prefix))
			{
				count++;
			}
		}

		return count;
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		for (Versioned<DiaryRecord> item : samples)
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
		for (Versioned<DiaryRecord> item : samples)
		{
			if (item.getId().equals(id))
			{
				// FIXME: unlink data
				return new Versioned<DiaryRecord>(item);
			}
		}
		return null;
	}

	@Override
	public List<Versioned<DiaryRecord>> findByIdPrefix(String prefix)
	{
		List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();

		for (Versioned<DiaryRecord> rec : samples)
		{
			if (rec.getId().startsWith(prefix))
			{
				// FIXME: unlink data
				result.add(new Versioned<DiaryRecord>(rec));
			}

			if (result.size() > MAX_READ_ITEMS)
			{
				throw new TooManyItemsException("Too many items");
			}
		}

		sort(result);
		return result;
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date since) throws CommonServiceException
	{
		List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();

		for (Versioned<DiaryRecord> item : samples)
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
	public List<Versioned<DiaryRecord>> findPeriod(Date startTime, Date endTime, boolean includeRemoved)
			throws CommonServiceException
	{
		List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();

		for (Versioned<DiaryRecord> item : samples)
		{
			Date time = item.getData().getTime();

			if ((includeRemoved || !item.isDeleted()) && (time.after(startTime) && time.before(endTime)))
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

			for (Versioned<DiaryRecord> x : samples)
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
				samples.add(new Versioned<DiaryRecord>(item));
			}
		}
	}

	@Override
	public MerkleTree getHashTree()
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
