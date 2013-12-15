package org.bosik.compensation.persistence.dao.local;

import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.exceptions.StoreException;
import android.content.ContentResolver;

public class NewLocalFoodBaseDAO implements FoodBaseDAO
{
	public NewLocalFoodBaseDAO(ContentResolver resolver)
	{
		// TODO Auto-generated constructor stub
	}

	@Override
	public String add(FoodItem item) throws StoreException
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
	public List<FoodItem> findAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<FoodItem> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public FoodItem findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public FoodItem findById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void update(FoodItem item) throws ItemNotFoundException, StoreException
	{
		// TODO Auto-generated method stub

	}
}
