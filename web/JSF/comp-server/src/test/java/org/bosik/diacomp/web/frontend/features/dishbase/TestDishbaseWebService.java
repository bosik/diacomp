package org.bosik.diacomp.web.frontend.features.dishbase;

import org.bosik.diacomp.core.services.dishbase.DishBaseService;
import org.bosik.diacomp.core.services.dishbase.TestDishbaseServiceCommon;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;

public class TestDishbaseWebService extends TestDishbaseServiceCommon
{
	@Override
	protected DishBaseService getService()
	{
		String login = Config.getLogin();
		String pass = Config.getPassword();
		int apiVersion = Config.getAPICurrent();

		return new DishbaseRestClient(new AuthRestClient(), login, pass, apiVersion);
	}
}
