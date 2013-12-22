package org.bosik.compensation.fakes.dao;

import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.exceptions.StoreException;

public class FakeFoodBaseDAO implements FoodBaseDAO
{

	public String add(Versioned<FoodItem> item) throws StoreException
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

	public void update(Versioned<FoodItem> item) throws ItemNotFoundException, StoreException
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
