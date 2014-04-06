package org.bosik.diacomp.web.frontend.features.diary;

import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryServiceCommon;
import org.bosik.diacomp.core.services.diary.TestDiaryService;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.features.diary.DiaryRestClient;

public class TestDiaryWebService extends TestDiaryServiceCommon implements TestDiaryService
{
	@Override
	protected DiaryService getService()
	{
		String login = Config.getTestLogin();
		String pass = Config.getTestPassword();
		int apiVersion = Config.getAPICurrent();

		return new DiaryRestClient(new AuthRestClient(), login, pass, apiVersion);
	}
}
