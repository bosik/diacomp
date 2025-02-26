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
package org.bosik.diacomp.core.services.fakes;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.mocks.Mock;
import org.bosik.diacomp.core.mocks.MockFoodItem;
import org.bosik.diacomp.core.mocks.MockVersionedConverter;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class FakeFoodBaseService implements FoodBaseService
{
	private final Mock<Versioned<FoodItem>>	mock			= new MockVersionedConverter<>(new MockFoodItem());
	private final List<Versioned<FoodItem>>	samples			= mock.getSamples();

	private static final int				MAX_READ_ITEMS	= 500;

	private static void sort(List<Versioned<FoodItem>> items)
	{
		items.sort((o1, o2) -> {
			String t1 = o1.getData().getName();
			String t2 = o2.getData().getName();
			return t1.compareTo(t2);
		});
	}

	@SuppressWarnings("unchecked")
	@Override
	public void add(Versioned<FoodItem> item) throws DuplicateException, PersistenceException
	{
		save(Arrays.asList(item));
	}

	@Override
	public int count(String prefix)
	{
		int count = 0;

		for (Versioned<FoodItem> item : samples)
		{
			if (item.getId().startsWith(prefix))
			{
				count++;
			}
		}

		return count;
	}

	@Override
	public void delete(String id)
	{
		synchronized (samples)
		{
			for (Versioned<FoodItem> rec : samples)
			{
				if (rec.getId().equals(id))
				{
					rec.setDeleted(true);
					return;
				}
			}
		}
	}

	@Override
	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		List<Versioned<FoodItem>> result = new ArrayList<>();

		for (Versioned<FoodItem> rec : samples)
		{
			if (includeRemoved || !rec.isDeleted())
			{
				result.add(new Versioned<>(rec));
			}
		}

		sort(result);
		return result;
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		List<Versioned<FoodItem>> result = new ArrayList<>();
		filter = filter.toLowerCase();

		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getData().getName().toLowerCase().contains(filter))
			{
				result.add(new Versioned<>(rec));
			}
		}

		sort(result);
		return result;
	}

	@Override
	public Versioned<FoodItem> findById(String id)
	{
		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getId().equals(id))
			{
				return new Versioned<>(rec);
			}
		}

		return null;
	}

	@Override
	public List<Versioned<FoodItem>> findByIdPrefix(String prefix)
	{
		List<Versioned<FoodItem>> result = new ArrayList<>();

		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getId().startsWith(prefix))
			{
				result.add(new Versioned<>(rec));
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
	public List<Versioned<FoodItem>> findChanged(Date since)
	{
		List<Versioned<FoodItem>> result = new ArrayList<>();

		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getTimeStamp().after(since))
			{
				result.add(new Versioned<>(rec));
			}
		}

		sort(result);
		return result;
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getData().getName().equals(exactName))
			{
				return new Versioned<>(rec);
			}
		}

		return null;
	}

	@Override
	public void save(List<Versioned<FoodItem>> items)
	{
		synchronized (samples)
		{
			for (Versioned<FoodItem> item : items)
			{
				boolean found = false;
				for (int i = 0; i < samples.size(); i++)
				{
					if (samples.get(i).equals(item))
					{
						samples.set(i, new Versioned<>(item));
						found = true;
						break;
					}
				}

				if (!found)
				{
					samples.add(new Versioned<>(item));
				}
			}
		}
	}

	@Override
	public String getHash(String prefix)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public Map<String, String> getHashChildren(String prefix)
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
