package org.bosik.diacomp.features.diary;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import org.bosik.diacomp.features.auth.AuthWebService;
import org.bosik.diacomp.features.auth.TestAuthWebService;
import org.bosik.diacomp.features.diary.DiaryWebService;
import org.bosik.diacomp.services.AuthService;
import org.bosik.diacomp.services.DiaryService;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.junit.Test;

public class TestDiaryWebService
{
	private AuthService		authService		= new AuthWebService();
	private DiaryService	diaryService	= new DiaryWebService();

	private void login()
	{
		authService.login(TestAuthWebService.TEST_LOGIN, TestAuthWebService.TEST_PASSWORD,
				TestAuthWebService.TEST_API_VERSION_CURRENT);
	}

	/**
	 * _simple-tests doesn't check any business logic. The only aspect the test is ability to invoke the method
	 */

	@Test(expected = NotAuthorizedException.class)
	public void test_Unauth_getRecords_Guids()
	{
		authService.logout();
		diaryService.getRecords(Collections.<String> emptyList());
	}

	@Test(expected = NotAuthorizedException.class)
	public void test_Unauth_getRecords_New()
	{
		authService.logout();
		diaryService.getRecords(new Date(), true);
	}

	@Test(expected = NotAuthorizedException.class)
	public void test_Unauth_getRecords_Period()
	{
		authService.logout();
		diaryService.getRecords(new Date(), new Date(), true);
	}

	@Test
	public void test_simple_Guids()
	{
		login();
		diaryService.getRecords(Arrays.<String> asList("abc", "def"));
	}

	@Test
	public void test_simple_New()
	{
		login();
		diaryService.getRecords(new Date(), true);
		diaryService.getRecords(new Date(), false);
	}

	@Test
	public void test_simple_Period()
	{
		login();
		diaryService.getRecords(new Date(), new Date(), true);
		diaryService.getRecords(new Date(), new Date(), false);
	}
}
