package org.bosik.diacomp.web.backend.features.diary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.bo.diary.DiaryRecord;
import org.bosik.diacomp.core.fakes.mocks.Mock;
import org.bosik.diacomp.core.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.core.persistence.common.Versioned;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.DiaryService;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.auth.rest.AuthRestClient;
import org.bosik.diacomp.web.backend.features.diary.rest.DiaryRestClient;
import org.bosik.diacomp.web.backend.utils.Config;
import org.junit.Test;

public class TestDiaryWebService
{
	private AuthService		authService		= new AuthRestClient();
	private DiaryService	diaryService	= new DiaryRestClient();

	private void login()
	{
		authService.login(Config.getLogin(), Config.getPassword(), Config.getAPICurrent());
	}

	/**
	 * _simple-tests doesn't check any business logic. The only aspect the test is ability to invoke
	 * the method
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

	@Test
	public void test_Post_insert()
	{
		Mock<DiaryRecord> mockRecord = new MockDiaryRecord();
		Mock<Versioned<DiaryRecord>> mockVersioned = new MockVersionedConverter<DiaryRecord>(mockRecord);
		List<Versioned<DiaryRecord>> items = mockVersioned.getSamples();

		login();

		// Insertion test

		diaryService.postRecords(items);
		for (Versioned<DiaryRecord> item : items)
		{
			List<Versioned<DiaryRecord>> restored = diaryService.getRecords(Arrays.<String> asList(item.getId()));
			assertNotNull(restored);
			assertEquals(1, restored.size());

			mockVersioned.compare(item, restored.get(0));
		}
	}

	@Test
	public void test_Post_update()
	{
		Mock<DiaryRecord> mockRecord = new MockDiaryRecord();
		Mock<Versioned<DiaryRecord>> mockVersioned = new MockVersionedConverter<DiaryRecord>(mockRecord);
		List<Versioned<DiaryRecord>> items = mockVersioned.getSamples();

		login();

		// Insertion test

		diaryService.postRecords(items);
		for (Versioned<DiaryRecord> item : items)
		{
			List<Versioned<DiaryRecord>> restored = diaryService.getRecords(Arrays.<String> asList(item.getId()));
			assertNotNull(restored);
			assertEquals(1, restored.size());

			mockVersioned.compare(item, restored.get(0));
		}

		// Updating test

		for (Versioned<DiaryRecord> item : items)
		{
			item.getData().setTime(new Date());
			item.setDeleted(item.isDeleted());
			item.setTimeStamp(new Date());
			item.setVersion(item.getVersion() + 1);
		}

		diaryService.postRecords(items);

		for (Versioned<DiaryRecord> item : items)
		{
			List<Versioned<DiaryRecord>> restored = diaryService.getRecords(Arrays.<String> asList(item.getId()));
			assertNotNull(restored);
			assertEquals(1, restored.size());

			mockVersioned.compare(item, restored.get(0));
		}
	}
}
