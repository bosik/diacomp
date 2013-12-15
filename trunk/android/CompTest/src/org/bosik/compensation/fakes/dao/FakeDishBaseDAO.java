package org.bosik.compensation.fakes.dao;

import java.util.List;
import org.bosik.compensation.bo.basic.Unique;
import org.bosik.compensation.bo.dishbase.DishItem;
import org.bosik.compensation.persistence.dao.DishBaseDAO;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.exceptions.StoreException;

public class FakeDishBaseDAO implements DishBaseDAO
{

	public String add(Unique<DishItem> item) throws StoreException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void delete(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	public List<Unique<DishItem>> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Unique<DishItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Unique<DishItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Unique<DishItem> findById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void update(Unique<DishItem> item) throws ItemNotFoundException, StoreException
	{
		// TODO Auto-generated method stub

	}

}
