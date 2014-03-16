package org.bosik.diacomp.android.backend.features.foodbase;

import org.bosik.diacomp.android.backend.common.webclient.TestWebClient;
import org.bosik.diacomp.core.persistence.serializers.ready.SerializerFoodItem;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.core.services.foodbase.TestFoodbaseServiceCommon;

public class TestWebFoodBaseService extends TestFoodbaseServiceCommon
{
	@Override
	protected FoodBaseService getService()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		return new FoodbaseRestClient(TestWebClient.getWebClient(), new SerializerFoodItem());
	}
}
