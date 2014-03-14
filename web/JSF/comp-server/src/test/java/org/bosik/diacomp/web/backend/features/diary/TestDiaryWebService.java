package org.bosik.diacomp.web.backend.features.diary;

import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryServiceContract;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.features.diary.DiaryRestClient;

public class TestDiaryWebService extends TestDiaryService implements TestDiaryServiceContract
{
	@Override
	protected DiaryService getService()
	{
		String login = Config.getLogin();
		String pass = Config.getPassword();
		int apiVersion = Config.getAPICurrent();

		return new DiaryRestClient(new AuthRestClient(), login, pass, apiVersion);
	}
}
