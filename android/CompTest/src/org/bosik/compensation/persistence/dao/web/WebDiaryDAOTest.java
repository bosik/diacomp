package org.bosik.compensation.persistence.dao.web;

import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.DiaryDAOTest;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClientTest;

public class WebDiaryDAOTest extends DiaryDAOTest
{
	private static final DiaryDAO	diaryDAO	= new WebDiaryDAO(WebClientTest.webClient);

	@Override
	protected DiaryDAO getDAO()
	{
		return diaryDAO;
	}
}
