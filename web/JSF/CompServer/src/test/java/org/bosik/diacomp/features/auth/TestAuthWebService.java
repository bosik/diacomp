package org.bosik.diacomp.features.auth;

import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.UnsupportedAPIException;
import org.bosik.diacomp.web.backend.features.auth.rest.AuthRestClient;
import org.bosik.diacomp.web.backend.utils.Config;
import org.junit.Test;

public class TestAuthWebService
{
	public static final String	TEST_LOGIN					= Config.getLogin();
	public static final String	TEST_PASSWORD				= Config.getPassword();
	public static final int		TEST_API_VERSION_CURRENT	= Config.getAPICurrent();
	public static final int		TEST_API_VERSION_SUPPORTED	= Config.getAPISupported();

	private AuthService			authService					= new AuthRestClient();

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
