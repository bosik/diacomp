package org.bosik.compensation.fakes.dao;

import java.util.List;
import org.bosik.compensation.bo.basic.Unique;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.exceptions.StoreException;

public class FakeFoodBaseDAO implements FoodBaseDAO
{

	public String add(Unique<FoodItem> item) throws StoreException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void delete(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	public List<Unique<FoodItem>> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Unique<FoodItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Unique<FoodItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Unique<FoodItem> findById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void update(Unique<FoodItem> item) throws ItemNotFoundException, StoreException
	{
		// TODO Auto-generated method stub

	}
}
