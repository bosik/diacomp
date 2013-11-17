package org.bosik.compensation.persistence.repository.diary;

import org.bosik.compensation.persistence.repository.providers.WebClient;
import org.bosik.compensation.persistence.repository.providers.WebClientTest;

public class WebDiaryRepositoryTest extends DiaryRepositoryTest
{
	// private static final String TAG = WebDiaryRepositoryTest.class.getSimpleName();
	private static final WebClient			webClient;
	private static final DiaryRepository	repository;

	static
	{
		webClient = WebClientTest.getWebClient();
		webClient.login();
		repository = new WebDiaryRepository(webClient);
	}

	@Override
	protected DiaryRepository getRepository()
	{
		return repository;
	}
}
