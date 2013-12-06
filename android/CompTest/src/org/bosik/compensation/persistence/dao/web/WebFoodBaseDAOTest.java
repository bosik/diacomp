package org.bosik.compensation.persistence.dao.web;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAOTest;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClientTest;
import org.bosik.compensation.persistence.serializers.foodbase.FoodBaseXMLSerializer;

public class WebFoodBaseDAOTest extends FoodBaseDAOTest
{
	@Override
	protected BaseDAO<FoodItem> getDAO()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		return new WebFoodBaseDAO(WebClientTest.getWebClient(), new FoodBaseXMLSerializer());
	}
}
