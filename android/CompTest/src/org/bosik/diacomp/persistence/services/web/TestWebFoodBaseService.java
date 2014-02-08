package org.bosik.diacomp.persistence.services.web;

import org.bosik.diacomp.persistence.services.TestFoodBaseService;
import org.bosik.diacomp.services.FoodBaseService;

public class TestWebFoodBaseService extends TestFoodBaseService
{
	@Override
	protected FoodBaseService getService()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		// , new SerializerFoodBaseXML()
		//
		// return new WebFoodBaseService(TestWebClient.getWebClient(), new Food);
		return null;
	}
}
