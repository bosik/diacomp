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
	private static final String	demoLogin		= "admin";
	private static final String	demoPass		= "1234";
	private static final int	apiVersion		= 20;

	private AuthService			authService		= new AuthWebService();
	private DiaryService		diaryService	= new DiaryWebService();

	@Test
	public void testLogin_correct()
	{
		// No exceptions will be thrown if auth is done ok
		authService.login(demoLogin, demoPass, apiVersion);
		diaryService.getRecords(new Date());
	}

	@Test(expected = NotAuthorizedException.class)
	public void testLogin_wrongPass()
	{
		authService.login(demoLogin, demoPass + "#:&%$'\"/\\", apiVersion);
	}

	@Test(expected = NotAuthorizedException.class)
	public void testLogin_wrongLogin()
	{
		authService.login(demoLogin + "#:&%$'\"/\\", demoPass, apiVersion);
	}

	@Test(expected = UnsupportedAPIException.class)
	public void testLogin_unsupportedApi()
	{
		authService.login(demoLogin, demoPass, 18);
	}

	@Test(expected = DeprecatedAPIException.class)
	public void testLogin_deprecatedApi()
	{
		authService.login(demoLogin, demoPass, 19);
	}

	public void testLogout()
	{
		authService.logout();
	}

	@Test(expected = NotAuthorizedException.class)
	public void testUnauthAccess()
	{
		authService.logout();
		diaryService.getRecords(new Date());
	}
}
