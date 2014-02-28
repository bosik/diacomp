package org.bosik.diacomp.android.persistence.services.local;

import java.util.List;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.ItemNotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;

public class LocalFoodBaseService implements FoodBaseService
{
	@Override
	public String add(Versioned<FoodItem> item) throws PersistenceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void delete(String id) throws ItemNotFoundException, AlreadyDeletedException
	{
		// TODO Auto-generated method stub

	}

	@Override
	public List<Versioned<FoodItem>> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<FoodItem>> findSysAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findSysById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void update(Versioned<FoodItem> item) throws ItemNotFoundException, PersistenceException
	{
		// TODO Auto-generated method stub

	}
}