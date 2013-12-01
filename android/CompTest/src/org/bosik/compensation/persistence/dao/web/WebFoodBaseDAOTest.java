package org.bosik.compensation.persistence.dao.web;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAOTest;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClientTest;

public class WebFoodBaseDAOTest extends FoodBaseDAOTest
{
	private static final BaseDAO<FoodItem>	foodBaseDAO	= new WebFoodBaseDAO(WebClientTest.webClient);

	@Override
	protected BaseDAO<FoodItem> getDAO()
	{
		return foodBaseDAO;
	}
}
