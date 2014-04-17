package org.bosik.diacomp.core.services;

import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryServiceCommon;
import org.bosik.diacomp.core.test.fakes.services.FakeDiaryService;

public class TestFakeDiaryService extends TestDiaryServiceCommon
{
	private static final DiaryService	service	= new FakeDiaryService();

	@Override
	protected DiaryService getService()
	{
		return service;
	}
}
