package org.bosik.compensation.persistence.repository.diary;

import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.web.WebDiaryDAO;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClient;
import org.bosik.compensation.persistence.repository.providers.WebClientTest;

public class WebDiaryRepositoryTest extends DiaryRepositoryTest
{
	private static final WebClient	webClient;
	private static final DiaryDAO	repository;

	static
	{
		webClient = WebClientTest.getWebClient();
		webClient.login();
		repository = new WebDiaryDAO(webClient);
	}

	@Override
	protected DiaryDAO getRepository()
	{
		return repository;
	}
}
