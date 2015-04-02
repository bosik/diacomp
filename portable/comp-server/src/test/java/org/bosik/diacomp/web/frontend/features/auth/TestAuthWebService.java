/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.frontend.features.auth;

public class TestAuthWebService
{
	// TODO: implement REST testing

	//	public static final String	TEST_LOGIN					= Config.getTestLogin();
	//	public static final String	TEST_PASSWORD				= Config.getTestPassword();
	//	public static final int		TEST_API_VERSION_CURRENT	= Config.getAPICurrent();
	//	public static final int		TEST_API_VERSION_SUPPORTED	= Config.getAPISupported();
	//
	//	private AuthService			authService					= new AuthRestClient();
	//
	//	@Test
	//	public void testLogin_correct()
	//	{
	//		authService.login(TEST_LOGIN, TEST_PASSWORD, TEST_API_VERSION_CURRENT);
	//	}
	//
	//	@Test(expected = NotAuthorizedException.class)
	//	public void testLogin_wrongPass()
	//	{
	//		authService.login(TEST_LOGIN, TEST_PASSWORD + "#:&%$'\"/\\", TEST_API_VERSION_CURRENT);
	//	}
	//
	//	@Test(expected = NotAuthorizedException.class)
	//	public void testLogin_wrongLogin()
	//	{
	//		authService.login(TEST_LOGIN + "#:&%$'\"/\\", TEST_PASSWORD, TEST_API_VERSION_CURRENT);
	//	}
	//
	//	@Test(expected = UnsupportedAPIException.class)
	//	public void testLogin_unsupportedApi()
	//	{
	//		authService.login(TEST_LOGIN, TEST_PASSWORD, TEST_API_VERSION_SUPPORTED - 1);
	//	}
	//
	//	@Test(expected = DeprecatedAPIException.class)
	//	public void testLogin_deprecatedApi()
	//	{
	//		authService.login(TEST_LOGIN, TEST_PASSWORD, TEST_API_VERSION_SUPPORTED);
	//	}
	//
	//	@Test
	//	public void testLogout()
	//	{
	//		authService.logout();
	//	}

	//	@Test(expected = NotAuthorizedException.class)
	//	public void getRecordsViaGuids_Unauth_Exception()
	//	{
	//		authService.logout();
	//		diaryService.getRecords(Collections.<String> emptyList());
	//	}
	//
	//	@Test(expected = NotAuthorizedException.class)
	//	public void getRecordsNew_Unauth_Exception()
	//	{
	//		authService.logout();
	//		diaryService.getRecords(new Date(), true);
	//	}
	//
	//	@Test(expected = NotAuthorizedException.class)
	//	public void getRecordsPeriod_Unauth_Exception()
	//	{
	//		authService.logout();
	//		diaryService.getRecords(new Date(), new Date(), true);
	//	}
}
