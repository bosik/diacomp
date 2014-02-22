package org.bosik.diacomp.fakes.services;

import java.util.List;
import org.bosik.diacomp.core.bo.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.common.Versioned;
import org.bosik.diacomp.core.services.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.ItemNotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;

public class FakeFoodBaseService implements FoodBaseService
{
	public String add(Versioned<FoodItem> item) throws PersistenceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void delete(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	public List<Versioned<FoodItem>> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Versioned<FoodItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Versioned<FoodItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Versioned<FoodItem> findById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void update(Versioned<FoodItem> item) throws ItemNotFoundException, PersistenceException
	{
		// TODO Auto-generated method stub

	}

	public List<Versioned<FoodItem>> findSysAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Versioned<FoodItem> findSysById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}
}
