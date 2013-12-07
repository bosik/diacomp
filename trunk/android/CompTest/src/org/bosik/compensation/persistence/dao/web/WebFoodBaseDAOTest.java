package org.bosik.compensation.persistence.dao.web;

import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAOTest;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClientTest;
import org.bosik.compensation.persistence.serializers.foodbase.FoodBaseXMLSerializer;

public class WebFoodBaseDAOTest extends FoodBaseDAOTest
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		return new WebFoodBaseDAO(WebClientTest.getWebClient(), new FoodBaseXMLSerializer());
	}
}
