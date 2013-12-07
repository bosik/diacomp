package org.bosik.compensation.fakes.dao;

import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.exceptions.DuplicateException;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;

public class FakeFoodBaseDAO implements FoodBaseDAO
{

	public String add(FoodItem item) throws DuplicateException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void delete(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	public List<FoodItem> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<FoodItem> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public FoodItem findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public FoodItem findById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void replaceAll(List<FoodItem> newList, int newVersion)
	{
		// TODO Auto-generated method stub

	}

	public void update(FoodItem item) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	public int getVersion()
	{
		// TODO Auto-generated method stub
		return 0;
	}

}
