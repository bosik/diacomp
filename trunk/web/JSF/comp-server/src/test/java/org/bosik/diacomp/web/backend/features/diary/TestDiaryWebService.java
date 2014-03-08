package org.bosik.diacomp.web.backend.features.diary;

import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryService;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.frontend.features.auth.AuthRememberService;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.features.diary.DiaryAuthorizedService;
import org.bosik.diacomp.web.frontend.features.diary.DiaryRestClient;

public class TestDiaryWebService extends TestDiaryService
{
	@Override
	protected DiaryService getService()
	{
		String login = Config.getLogin();
		String pass = Config.getPassword();
		int apiVersion = Config.getAPICurrent();
		AuthRememberService authService = new AuthRememberService(new AuthRestClient(), login, pass, apiVersion);

		return new DiaryAuthorizedService(new DiaryRestClient(), authService);
	}
}
