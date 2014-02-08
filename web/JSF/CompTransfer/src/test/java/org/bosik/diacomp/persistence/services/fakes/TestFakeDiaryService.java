package org.bosik.diacomp.persistence.services.fakes;

import org.bosik.diacomp.fakes.services.FakeDiaryService;
import org.bosik.diacomp.persistence.services.TestDiaryService;
import org.junit.Ignore;
import services.DiaryService;

@Ignore
public class TestFakeDiaryService extends TestDiaryService
{
	private static DiaryService	service	= new FakeDiaryService();

	@Override
	protected DiaryService getService()
	{
		return service;
	}
}
