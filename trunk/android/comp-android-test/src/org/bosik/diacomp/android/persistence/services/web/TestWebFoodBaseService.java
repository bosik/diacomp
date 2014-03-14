package org.bosik.diacomp.android.persistence.services.web;

import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.core.services.foodbase.TestFoodbaseServiceCommon;

public class TestWebFoodBaseService extends TestFoodbaseServiceCommon
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
