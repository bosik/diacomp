package org.bosik.compensation.fakes.dao;

import java.util.List;
import org.bosik.compensation.bo.dishbase.DishItem;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.dao.DishBaseDAO;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.exceptions.StoreException;

public class FakeDishBaseDAO implements DishBaseDAO
{
	public String add(Versioned<DishItem> item) throws StoreException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void delete(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	public List<Versioned<DishItem>> findAll(boolean includeDeleted)
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

	public void update(Versioned<DishItem> item) throws ItemNotFoundException, StoreException
	{
		// TODO Auto-generated method stub

	}

}
