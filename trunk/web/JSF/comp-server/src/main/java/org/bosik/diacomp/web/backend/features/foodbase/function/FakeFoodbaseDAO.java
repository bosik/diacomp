package org.bosik.diacomp.web.backend.features.foodbase.function;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;

public class FakeFoodbaseDAO implements FoodbaseDAO
{
	private static Mock<Versioned<FoodItem>>	mock	= new MockVersionedConverter<FoodItem>(new MockFoodItem());
	private static List<Versioned<FoodItem>>	samples	= new ArrayList<Versioned<FoodItem>>();
	{
		for (int i = 0; i < 100; i++)
		{
			Versioned<FoodItem> sample = mock.getSample();
			sample.setDeleted(false);
			samples.add(sample);
		}
	}

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

	@Override
	public List<Versioned<FoodItem>> findAll(int userId, boolean includeRemoved)
	{
		List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();

		for (Versioned<FoodItem> rec : samples)
		{
			final FoodItem data = rec.getData();
			if (includeRemoved || !rec.isDeleted())
			{
				Versioned<FoodItem> item = new Versioned<FoodItem>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(data);
				result.add(item);
			}
		}

		sort(result);
		return result;
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(int userId, Date since)
	{
		List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();

		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getTimeStamp().after(since))
			{
				Versioned<FoodItem> item = new Versioned<FoodItem>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(rec.getData());
				result.add(item);
			}
		}

		sort(result);
		return result;
	}

	@Override
	public Versioned<FoodItem> findById(int userId, String guid)
	{
		for (Versioned<FoodItem> rec : samples)
		{
			final FoodItem data = rec.getData();
			if (rec.getId().equals(guid))
			{
				Versioned<FoodItem> item = new Versioned<FoodItem>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(data);
				return item;
			}
		}

		return null;
	}

	@Override
	public void save(int userId, List<Versioned<FoodItem>> items)
	{
		for (Versioned<FoodItem> item : samples)
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

	@Override
	public List<Versioned<FoodItem>> findAny(int userId, String filter)
	{
		List<Versioned<FoodItem>> result = new ArrayList<Versioned<FoodItem>>();

		for (Versioned<FoodItem> rec : samples)
		{
			if (rec.getData().getName().contains(filter))
			{
				Versioned<FoodItem> item = new Versioned<FoodItem>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(rec.getData());
				result.add(item);
			}
		}

		sort(result);
		return result;
	}

	@Override
	public Versioned<FoodItem> findOne(int userId, String exactName)
	{
		for (Versioned<FoodItem> rec : samples)
		{
			final FoodItem data = rec.getData();
			if (rec.getData().getName().equals(exactName))
			{
				Versioned<FoodItem> item = new Versioned<FoodItem>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(data);
				return item;
			}
		}

		return null;
	}

	@Override
	public void delete(int userId, String id)
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
