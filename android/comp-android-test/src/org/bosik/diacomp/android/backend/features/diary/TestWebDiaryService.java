package org.bosik.diacomp.android.backend.features.diary;

import org.bosik.diacomp.android.backend.common.webclient.TestWebClient;
import org.bosik.diacomp.android.backend.features.diary.DiaryWebService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryServiceCommon;
import org.bosik.diacomp.core.services.diary.TestDiaryService;

public class TestWebDiaryService extends TestDiaryServiceCommon implements TestDiaryService
{
	@Override
	protected DiaryService getService()
	{
		// DO NOT MAKE IT STATIC - IT CAUSES android.os.NetworkOnMainThreadException
		return new DiaryWebService(TestWebClient.getWebClient());
	}
}
