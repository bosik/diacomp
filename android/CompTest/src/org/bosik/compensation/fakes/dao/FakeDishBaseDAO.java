package org.bosik.compensation.fakes.dao;

import java.util.List;
import org.bosik.compensation.bo.dishbase.DishItem;
import org.bosik.compensation.persistence.dao.DishBaseDAO;
import org.bosik.compensation.persistence.exceptions.DuplicateException;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;

public class FakeDishBaseDAO implements DishBaseDAO
{
	public String add(DishItem item) throws DuplicateException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void delete(String id) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	public List<DishItem> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<DishItem> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public DishItem findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public DishItem findById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void replaceAll(List<DishItem> newList, int newVersion)
	{
		// TODO Auto-generated method stub

	}

	public void update(DishItem item) throws ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	public int getVersion()
	{
		// TODO Auto-generated method stub
		return 0;
	}
}
