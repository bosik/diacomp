package org.bosik.diacomp.android.persistence.services.web;

import org.bosik.diacomp.core.persistence.services.TestFoodBaseService;
import org.bosik.diacomp.core.services.FoodBaseService;

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
