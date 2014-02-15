package org.bosik.diacomp.features.auth;

import org.bosik.diacomp.features.auth.AuthWebService;
import org.bosik.diacomp.services.AuthService;
import org.bosik.diacomp.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.services.exceptions.UnsupportedAPIException;
import org.junit.Test;

public class TestAuthWebService
{
	public static final String	TEST_LOGIN					= "admin";
	public static final String	TEST_PASSWORD				= "1234";
	public static final int		TEST_API_VERSION_CURRENT	= 20;
	public static final int		TEST_API_VERSION_SUPPORTED	= 19;

	private AuthService			authService					= new AuthWebService();

	@Test
	public void testLogin_correct()
	{
		authService.login(TEST_LOGIN, TEST_PASSWORD, TEST_API_VERSION_CURRENT);
	}

	@Test(expected = NotAuthorizedException.class)
	public void testLogin_wrongPass()
	{
		authService.login(TEST_LOGIN, TEST_PASSWORD + "#:&%$'\"/\\", TEST_API_VERSION_CURRENT);
	}

	@Test(expected = NotAuthorizedException.class)
	public void testLogin_wrongLogin()
	{
		authService.login(TEST_LOGIN + "#:&%$'\"/\\", TEST_PASSWORD, TEST_API_VERSION_CURRENT);
	}

	@Test(expected = UnsupportedAPIException.class)
	public void testLogin_unsupportedApi()
	{
		authService.login(TEST_LOGIN, TEST_PASSWORD, TEST_API_VERSION_SUPPORTED - 1);
	}

	@Test(expected = DeprecatedAPIException.class)
	public void testLogin_deprecatedApi()
	{
		authService.login(TEST_LOGIN, TEST_PASSWORD, TEST_API_VERSION_SUPPORTED);
	}

	@Test
	public void testLogout()
	{
		authService.logout();
	}
}
