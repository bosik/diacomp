package org.bosik.diacomp.web.backend.features.base.dish.function;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.test.fakes.services.FakeDishBaseService;

/**
 * NOTE: ignores userId
 * 
 * @author Bosik
 * 
 */
public class FakeDishbaseDAO implements DishbaseDAO
{
	private DishBaseService	dishbaseService	= new FakeDishBaseService();

	@Override
	public void delete(int userId, String id)
	{
		dishbaseService.delete(id);
	}

	@Override
	public List<Versioned<DishItem>> findAll(int userId, boolean includeRemoved)
	{
		return dishbaseService.findAll(includeRemoved);
	}

	@Override
	public List<Versioned<DishItem>> findAny(int userId, String filter)
	{
		return dishbaseService.findAny(filter);
	}

	@Override
	public Versioned<DishItem> findById(int userId, String id)
	{
		return dishbaseService.findById(id);
	}

	@Override
	public List<Versioned<DishItem>> findByIdPrefix(int userId, String prefix)
	{
		return dishbaseService.findByIdPrefix(prefix);
	}

	@Override
	public List<Versioned<DishItem>> findChanged(int userId, Date since)
	{
		return dishbaseService.findChanged(since);
	}

	@Override
	public Versioned<DishItem> findOne(int userId, String exactName)
	{
		return dishbaseService.findOne(exactName);
	}

	@Override
	public void save(int userId, List<Versioned<DishItem>> items)
	{
		dishbaseService.save(items);
	}
}
