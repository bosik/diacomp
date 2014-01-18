package org.bosik.diacomp.persistence.dao.local;

import java.util.List;
import org.bosik.compensation.bo.dishbase.DishItem;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.dao.DishBaseDAO;
import org.bosik.compensation.persistence.exceptions.AlreadyDeletedException;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.exceptions.StoreException;

public class LocalDishBaseDAO implements DishBaseDAO
{

	@Override
	public String add(Versioned<DishItem> item) throws StoreException
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
	public List<Versioned<DishItem>> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<DishItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<DishItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<DishItem> findById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<DishItem>> findSysAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<DishItem> findSysById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void update(Versioned<DishItem> item) throws ItemNotFoundException, StoreException
	{
		// TODO Auto-generated method stub

	}

}