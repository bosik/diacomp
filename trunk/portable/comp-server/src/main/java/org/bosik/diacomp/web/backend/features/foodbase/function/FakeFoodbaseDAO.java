package org.bosik.diacomp.web.backend.features.foodbase.function;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.core.test.fakes.services.FakeFoodBaseService;

/**
 * NOTE: ignores userId
 * 
 * @author Bosik
 * 
 */
public class FakeFoodbaseDAO implements FoodbaseDAO
{
	private FoodBaseService	foodbaseService	= new FakeFoodBaseService();

	@Override
	public void delete(int userId, String id)
	{
		foodbaseService.delete(id);
	}

	@Override
	public List<Versioned<FoodItem>> findAll(int userId, boolean includeRemoved)
	{
		return foodbaseService.findAll(includeRemoved);
	}

	@Override
	public List<Versioned<FoodItem>> findAny(int userId, String filter)
	{
		return foodbaseService.findAny(filter);
	}

	@Override
	public Versioned<FoodItem> findById(int userId, String id)
	{
		return foodbaseService.findById(id);
	}

	@Override
	public List<Versioned<FoodItem>> findByIdPrefix(int userId, String prefix)
	{
		return foodbaseService.findByIdPrefix(prefix);
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(int userId, Date since)
	{
		return foodbaseService.findChanged(since);
	}

	@Override
	public Versioned<FoodItem> findOne(int userId, String exactName)
	{
		return foodbaseService.findOne(exactName);
	}

	@Override
	public void save(int userId, List<Versioned<FoodItem>> items)
	{
		foodbaseService.save(items);
	}
}
