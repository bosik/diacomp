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
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;

public class FakeObjectService implements ObjectService<String>
{
	private final List<Versioned<String>> data = new ArrayList<Versioned<String>>();

	@Override
	public int count(String prefix)
	{
		int count = 0;

		for (Versioned<String> item : data)
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
		for (Versioned<String> item : data)
		{
			if (item.getId().equals(id))
			{
				if (!item.isDeleted())
				{
					item.setDeleted(true);
					return;
				}
				else
				{
					throw new AlreadyDeletedException(id);
				}
			}
		}

		throw new NotFoundException(id);
	}

	@Override
	public Versioned<String> findById(String id) throws CommonServiceException
	{
		for (Versioned<String> item : data)
		{
			if (item.getId().equals(id))
			{
				return new Versioned<String>(item);
			}
		}
		return null;
	}

	@Override
	public List<Versioned<String>> findByIdPrefix(String prefix) throws CommonServiceException
	{
		List<Versioned<String>> result = new ArrayList<Versioned<String>>();

		for (Versioned<String> item : data)
		{
			if (item.getId().startsWith(prefix))
			{
				result.add(new Versioned<String>(item));
			}

			if (result.size() > MAX_ITEMS_COUNT)
			{
				throw new TooManyItemsException("Too many items");
			}
		}

		return result;
	}

	@Override
	public List<Versioned<String>> findChanged(Date since) throws CommonServiceException
	{
		List<Versioned<String>> result = new ArrayList<Versioned<String>>();

		for (Versioned<String> item : data)
		{
			if (item.getTimeStamp().after(since))
			{
				result.add(new Versioned<String>(item));
			}
		}

		return result;
	}

	@Override
	public void save(List<Versioned<String>> items) throws CommonServiceException
	{
		for (Versioned<String> item : items)
		{
			Versioned<String> temp = findById(item.getId());

			if (temp == null)
			{
				data.add(new Versioned<String>(item));
			}
			else
			{
				for (Versioned<String> x : data)
				{
					if (x.getId().equals(item.getId()))
					{
						x.setData(item.getData());
						x.setDeleted(item.isDeleted());
						x.setTimeStamp(item.getTimeStamp());
						x.setVersion(item.getVersion());
						break;
					}
				}

			}
		}
	}

	@Override
	public MerkleTree getHashTree()
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
