package org.bosik.diacomp.frontend.services;

import java.util.Date;

import org.bosik.diacomp.services.AuthService;
import org.bosik.diacomp.services.DiaryService;
import org.bosik.diacomp.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.services.exceptions.UnsupportedAPIException;
import org.junit.Test;

public class TestAuthWebService
{
	private static final String	TEST_LOGIN					= "admin";
	private static final String	TEST_PASSWORD				= "1234";
	private static final int	TEST_API_VERSION_CURRENT	= 20;
	private static final int	TEST_API_VERSION_SUPPORTED	= 19;

	private AuthService			authService					= new AuthWebService();
	private DiaryService		diaryService				= new DiaryWebService();

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

	@Test
	public void testAuthAccess()
	{
		authService.login(TEST_LOGIN, TEST_PASSWORD, TEST_API_VERSION_CURRENT);
		diaryService.getRecords(new Date(), true);
	}

	@Test(expected = NotAuthorizedException.class)
	public void testUnauthAccess()
	{
		authService.logout();
		diaryService.getRecords(new Date(), true);
	}
}
