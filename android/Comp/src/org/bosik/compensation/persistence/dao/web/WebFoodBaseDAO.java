package org.bosik.compensation.persistence.dao.web;

import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClient;

public class WebFoodBaseDAO implements BaseDAO<FoodItem>
{

	public WebFoodBaseDAO(WebClient webClient)
	{
		// TODO Auto-generated constructor stub
	}

	@Override
	public String add(FoodItem item) throws org.bosik.compensation.persistence.dao.BaseDAO.DuplicateException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void delete(String id) throws org.bosik.compensation.persistence.dao.BaseDAO.ItemNotFoundException
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
	public FoodItem findById(String id)
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
	public void replaceAll(List<FoodItem> newList, int newVersion)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void update(FoodItem item) throws org.bosik.compensation.persistence.dao.BaseDAO.ItemNotFoundException
	{
		// TODO Auto-generated method stub

	}

	@Override
	public int getVersion()
	{
		// TODO Auto-generated method stub
		return 0;
	}
}
