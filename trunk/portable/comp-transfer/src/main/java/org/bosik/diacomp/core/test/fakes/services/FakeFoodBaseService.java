package org.bosik.diacomp.core.test.fakes.services;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.services.sync.MerkleTree;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;

public class FakeFoodBaseService implements FoodBaseService
{
	private Mock<Versioned<FoodItem>>	mock	= new MockVersionedConverter<FoodItem>(new MockFoodItem());
	private List<Versioned<FoodItem>>	samples	= mock.getSamples();

	private static void sort(List<Versioned<FoodItem>> items)
	{
		Collections.sort(items, new Comparator<Versioned<FoodItem>>()
		{
			@Override
			public int compare(Versioned<FoodItem> o1, Versioned<FoodItem> o2)
			{
				String t1 = o1.getData().getName();
				String t2 = o2.getData().getName();
				return t1.compareTo(t2);
			}
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
		List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();

		for (Versioned<FoodItem> rec : samples)
		{
			if (includeRemoved || !rec.isDeleted())
			{
				result.add(new Versioned<FoodItem>(rec));
			}
		}

		sort(result);
		return result;
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();
		filter = filter.toLowerCase();

		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getData().getName().toLowerCase().contains(filter))
			{
				result.add(new Versioned<FoodItem>(rec));
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
				return new Versioned<FoodItem>(rec);
			}
		}

		return null;
	}

	@Override
	public List<Versioned<FoodItem>> findByIdPrefix(String prefix)
	{
		List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();

		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getId().startsWith(prefix))
			{
				result.add(new Versioned<FoodItem>(rec));
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
	public List<Versioned<FoodItem>> findChanged(Date since)
	{
		List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();

		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getTimeStamp().after(since))
			{
				result.add(new Versioned<FoodItem>(rec));
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
				return new Versioned<FoodItem>(rec);
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
						samples.set(i, new Versioned<FoodItem>(item));
						found = true;
						break;
					}
				}

				if (!found)
				{
					samples.add(new Versioned<FoodItem>(item));
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

	@Override
	public MerkleTree getHashTree()
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
