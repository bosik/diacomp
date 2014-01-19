package org.bosik.diacomp.persistence.dao.web;

import org.bosik.diacomp.persistence.dao.DiaryDAO;
import org.bosik.diacomp.persistence.dao.TestDiaryDAO;
import org.bosik.diacomp.persistence.dao.web.utils.client.TestWebClient;

public class TestWebDiaryDAO extends TestDiaryDAO
{
	@Override
	protected DiaryDAO getDAO()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		return new WebDiaryDAO(TestWebClient.getWebClient());
	}
}
