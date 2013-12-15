package org.bosik.compensation.persistence.dao.local;

import java.util.List;
import org.bosik.compensation.bo.basic.Unique;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.exceptions.StoreException;

public class LocalFoodBaseDAO implements FoodBaseDAO
{

	@Override
	public String add(Unique<FoodItem> item) throws StoreException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void delete(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	@Override
	public List<Unique<FoodItem>> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Unique<FoodItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Unique<FoodItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Unique<FoodItem> findById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void update(Unique<FoodItem> item) throws ItemNotFoundException, StoreException
	{
		// TODO Auto-generated method stub

	}

}