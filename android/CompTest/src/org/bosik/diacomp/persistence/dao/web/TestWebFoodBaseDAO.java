package org.bosik.diacomp.persistence.dao.web;

import org.bosik.diacomp.persistence.dao.FoodBaseDAO;
import org.bosik.diacomp.persistence.dao.TestFoodBaseDAO;

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
