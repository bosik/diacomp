package org.bosik.diacomp.web.frontend.features.foodbase;

import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.core.services.foodbase.TestFoodbaseServiceCommon;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.features.foodbase.FoodbaseRestClient;

public class TestFoodbaseWebService extends TestFoodbaseServiceCommon
{
	@Override
	protected FoodBaseService getService()
	{
		String login = Config.getTestLogin();
		String pass = Config.getTestPassword();
		int apiVersion = Config.getAPICurrent();

		return new FoodbaseRestClient(new AuthRestClient(), login, pass, apiVersion);
	}
}
