package org.bosik.diacomp.frontend.services;

import org.bosik.diacomp.services.AuthService;
import org.bosik.diacomp.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.services.exceptions.UnsupportedAPIException;
import org.junit.Test;

public class TestAuthWebService
{
	private static final String	demoLogin	= "admin";
	private static final String	demoPass	= "1234";
	private static final int	apiVersion	= 20;

	private AuthService			authService	= new AuthWebService();

	@Test
	public void loginTest_correct()
	{
		// No exceptions will be thrown if auth is done ok
		authService.login(demoLogin, demoPass, apiVersion);
	}

	@Test(expected = NotAuthorizedException.class)
	public void loginTest_wrongPass()
	{
		authService.login(demoLogin, demoPass + "#:&%$'\"/\\", apiVersion);
	}

	@Test(expected = NotAuthorizedException.class)
	public void loginTest_wrongLogin()
	{
		authService.login(demoLogin + "#:&%$'\"/\\", demoPass, apiVersion);
	}

	@Test(expected = UnsupportedAPIException.class)
	public void loginTest_unsupportedApi()
	{
		authService.login(demoLogin, demoPass, 18);
	}

	@Test(expected = DeprecatedAPIException.class)
	public void loginTest_deprecatedApi()
	{
		authService.login(demoLogin, demoPass, 19);
	}

	public void logoutTest()
	{
		authService.logout();
	}
}
