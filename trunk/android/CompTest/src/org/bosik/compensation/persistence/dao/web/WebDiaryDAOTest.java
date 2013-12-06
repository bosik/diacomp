package org.bosik.compensation.persistence.dao.web;

import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.DiaryDAOTest;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClientTest;

public class WebDiaryDAOTest extends DiaryDAOTest
{
	@Override
	protected DiaryDAO getDAO()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		return new WebDiaryDAO(WebClientTest.getWebClient());
	}
}
