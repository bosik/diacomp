package org.bosik.diacomp.web.backend.features.dishbase.function;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockDishItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;

public class FakeDishbaseDAO implements DishbaseDAO
{
	private static Mock<Versioned<DishItem>>	mock	= new MockVersionedConverter<DishItem>(new MockDishItem());
	private static List<Versioned<DishItem>>	samples	= new ArrayList<Versioned<DishItem>>();
	static
	{
		samples.clear();
		for (int i = 0; i < 100; i++)
		{
			Versioned<DishItem> sample = mock.getSample();
			sample.setDeleted(false);
			samples.add(sample);
		}
	}

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

	@Override
	public List<Versioned<DishItem>> findAll(int userId, boolean includeRemoved)
	{
		List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();

		for (Versioned<DishItem> rec : samples)
		{
			final DishItem data = rec.getData();
			if (includeRemoved || !rec.isDeleted())
			{
				Versioned<DishItem> item = new Versioned<DishItem>();
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
	public List<Versioned<DishItem>> findChanged(int userId, Date since)
	{
		List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();

		for (Versioned<DishItem> rec : samples)
		{
			if (rec.getTimeStamp().after(since))
			{
				Versioned<DishItem> item = new Versioned<DishItem>();
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
	public Versioned<DishItem> findById(int userId, String guid)
	{
		for (Versioned<DishItem> rec : samples)
		{
			final DishItem data = rec.getData();
			if (rec.getId().equals(guid))
			{
				Versioned<DishItem> item = new Versioned<DishItem>();
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
	public void save(int userId, List<Versioned<DishItem>> items)
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
	public List<Versioned<DishItem>> findAny(int userId, String filter)
	{
		List<Versioned<DishItem>> result = new ArrayList<Versioned<DishItem>>();
		filter = filter.toLowerCase();

		for (Versioned<DishItem> rec : samples)
		{
			if (rec.getData().getName().toLowerCase().contains(filter))
			{
				Versioned<DishItem> item = new Versioned<DishItem>();
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
	public Versioned<DishItem> findOne(int userId, String exactName)
	{
		for (Versioned<DishItem> rec : samples)
		{
			final DishItem data = rec.getData();
			if (rec.getData().getName().equals(exactName))
			{
				Versioned<DishItem> item = new Versioned<DishItem>();
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
}
