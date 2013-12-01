package org.bosik.compensation.persistence.dao.web;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAOTest;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClientTest;
import org.bosik.compensation.persistence.serializers.foodbase.FoodBaseXMLSerializer;

public class WebFoodBaseDAOTest extends FoodBaseDAOTest
{
	private static final BaseDAO<FoodItem>	foodBaseDAO	= new WebFoodBaseDAO(WebClientTest.webClient,
																new FoodBaseXMLSerializer());

	@Override
	protected BaseDAO<FoodItem> getDAO()
	{
		return foodBaseDAO;
	}
}
