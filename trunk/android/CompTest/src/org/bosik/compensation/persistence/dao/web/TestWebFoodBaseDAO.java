package org.bosik.compensation.persistence.dao.web;

import org.bosik.compensation.persistence.dao.TestFoodBaseDAO;
import org.bosik.diacomp.persistence.dao.FoodBaseDAO;

public class TestWebFoodBaseDAO extends TestFoodBaseDAO
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		// , new SerializerFoodBaseXML()
		//
		// return new WebFoodBaseDAO(TestWebClient.getWebClient(), new Food);
		return null;
	}
}
