package org.bosik.diacomp.core.testutils.fakes.services;

import java.util.List;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.ItemNotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;

public class FakeDishBaseService implements DishBaseService
{
	public String add(Versioned<DishItem> item) throws PersistenceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void delete(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	public List<Versioned<DishItem>> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Versioned<DishItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Versioned<DishItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Versioned<DishItem> findById(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void update(Versioned<DishItem> item) throws ItemNotFoundException, PersistenceException
	{
		// TODO Auto-generated method stub

	}

	public List<Versioned<DishItem>> findSysAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Versioned<DishItem> findSysById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

}
