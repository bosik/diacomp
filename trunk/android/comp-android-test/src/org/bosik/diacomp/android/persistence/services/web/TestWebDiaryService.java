package org.bosik.diacomp.android.persistence.services.web;

import org.bosik.diacomp.android.backend.features.diary.WebDiaryService;
import org.bosik.diacomp.android.persistence.services.web.utils.client.TestWebClient;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryServiceContract;

public class TestWebDiaryService extends TestDiaryService implements TestDiaryServiceContract
{
	@Override
	protected DiaryService getService()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		return new WebDiaryService(TestWebClient.getWebClient());
	}
}
