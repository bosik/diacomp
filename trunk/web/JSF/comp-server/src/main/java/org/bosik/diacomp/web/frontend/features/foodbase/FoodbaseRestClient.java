package org.bosik.diacomp.web.frontend.features.foodbase;

import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.web.frontend.common.AuthorizedRestClient;

public class FoodbaseRestClient extends AuthorizedRestClient //implements FoodBaseService
{
	public FoodbaseRestClient(AuthService authService, String login, String pass, int apiVersion)
	{
		super(authService, login, pass, apiVersion);
	}
}
