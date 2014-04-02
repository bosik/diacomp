package org.bosik.diacomp.android.backend.features.dishbase;

import org.bosik.diacomp.android.backend.common.webclient.TestWebClient;
import org.bosik.diacomp.core.services.dishbase.DishBaseService;
import org.bosik.diacomp.core.services.dishbase.TestDishbaseServiceCommon;

public class TestDishBaseWebService extends TestDishbaseServiceCommon
{
	@Override
	protected DishBaseService getService()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		return new DishBaseWebService(TestWebClient.getWebClient());
	}
}
