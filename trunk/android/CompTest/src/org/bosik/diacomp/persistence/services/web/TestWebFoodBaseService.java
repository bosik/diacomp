package org.bosik.diacomp.persistence.services.web;

import org.bosik.diacomp.core.services.FoodBaseService;
import org.bosik.diacomp.persistence.services.TestFoodBaseService;

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
