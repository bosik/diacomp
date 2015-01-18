package org.bosik.diacomp.core.test.fakes.services;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockDishItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;

public class FakeDishBaseService implements DishBaseService
{
	private Mock<Versioned<DishItem>>	mock	= new MockVersionedConverter<DishItem>(new MockDishItem());
	private List<Versioned<DishItem>>	samples	= mock.getSamples();

	private static void sort(List<Versioned<DishItem>> items)
	{
		Collections.sort(items, new Comparator<Versioned<DishItem>>()
		{
			@Override
			public int compare(Versioned<DishItem> o1, Versioned<DishItem> o2)
			{
				String t1 = o1.getData().getName();
				String t2 = o2.getData().getName();
				return t1.compareTo(t2);
			}
		});
	}

	@SuppressWarnings("unchecked")
	@Override
	public void add(Versioned<DishItem> item) throws DuplicateException, PersistenceException
	{
		save(Arrays.asList(item));
	}

	@Override
	public int count(String prefix)
	{
		int count = 0;

		for (Versioned<DishItem> item : samples)
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
			for (Versioned<DishItem> rec : samples)
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
	public List<Versioned<DishItem>> findAll(boolean includeRemoved)
	{
		List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();

		for (Versioned<DishItem> rec : samples)
		{
			if (includeRemoved || !rec.isDeleted())
			{
				result.add(new Versioned<DishItem>(rec));
			}
		}

		sort(result);
		return result;
	}

	@Override
	public List<Versioned<DishItem>> findAny(String filter)
	{
		List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();
		filter = filter.toLowerCase();

		for (Versioned<DishItem> rec : samples)
		{
			if (rec.getData().getName().toLowerCase().contains(filter))
			{
				result.add(new Versioned<DishItem>(rec));
			}
		}

		sort(result);
		return result;
	}

	@Override
	public Versioned<DishItem> findById(String id)
	{
		for (Versioned<DishItem> rec : samples)
		{
			if (rec.getId().equals(id))
			{
				return new Versioned<DishItem>(rec);
			}
		}

		return null;
	}

	@Override
	public List<Versioned<DishItem>> findByIdPrefix(String prefix)
	{
		List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();

		for (Versioned<DishItem> rec : samples)
		{
			if (rec.getId().startsWith(prefix))
			{
				result.add(new Versioned<DishItem>(rec));
			}

			if (result.size() > MAX_ITEMS_COUNT)
			{
				throw new TooManyItemsException("Too many items");
			}
		}

		sort(result);
		return result;
	}

	@Override
	public List<Versioned<DishItem>> findChanged(Date since)
	{
		List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();

		for (Versioned<DishItem> rec : samples)
		{
			if (rec.getTimeStamp().after(since))
			{
				result.add(new Versioned<DishItem>(rec));
			}
		}

		sort(result);
		return result;
	}

	@Override
	public Versioned<DishItem> findOne(String exactName)
	{
		for (Versioned<DishItem> rec : samples)
		{
			if (rec.getData().getName().equals(exactName))
			{
				return new Versioned<DishItem>(rec);
			}
		}

		return null;
	}

	@Override
	public void save(List<Versioned<DishItem>> items)
	{
		synchronized (samples)
		{
			for (Versioned<DishItem> item : items)
			{
				boolean found = false;
				for (int i = 0; i < samples.size(); i++)
				{
					if (samples.get(i).equals(item))
					{
						samples.set(i, new Versioned<DishItem>(item));
						found = true;
						break;
					}
				}

				if (!found)
				{
					samples.add(new Versioned<DishItem>(item));
				}
			}
		}
	}

	@Override
	public String getHash(String prefix) throws CommonServiceException
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public Map<String, String> getHashChildren(String prefix) throws CommonServiceException
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public void setHash(String prefix, String hash)
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
