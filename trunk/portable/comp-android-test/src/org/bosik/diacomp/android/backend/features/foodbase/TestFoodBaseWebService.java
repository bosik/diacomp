package org.bosik.diacomp.android.backend.features.foodbase;

import org.bosik.diacomp.android.backend.common.webclient.TestWebClient;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.base.food.TestFoodbaseServiceCommon;

public class TestFoodBaseWebService extends TestFoodbaseServiceCommon
{
	@Override
	protected FoodBaseService getService()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		return new FoodBaseWebService(TestWebClient.getWebClient());
	}
}
