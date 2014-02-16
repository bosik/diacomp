package org.bosik.diacomp.features.auth;

import org.bosik.diacomp.services.AuthService;
import org.bosik.diacomp.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.services.exceptions.UnsupportedAPIException;
import org.bosik.diacomp.utils.Config;
import org.junit.Test;

public class TestAuthWebService
{
	public static final String	TEST_LOGIN					= Config.getLogin();
	public static final String	TEST_PASSWORD				= Config.getPassword();
	public static final int		TEST_API_VERSION_CURRENT	= Config.getAPICurrent();
	public static final int		TEST_API_VERSION_SUPPORTED	= Config.getAPISupported();

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
