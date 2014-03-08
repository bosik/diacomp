package org.bosik.diacomp.core.persistence.services.fakes;

import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryService;
import org.bosik.diacomp.core.services.diary.TestDiaryServiceContract;
import org.bosik.diacomp.core.utils.test.fakes.services.FakeDiaryService;
import org.junit.Ignore;

@Ignore
public class TestFakeDiaryService extends TestDiaryService implements TestDiaryServiceContract
{
	private static DiaryService	service	= new FakeDiaryService();

	@Override
	protected DiaryService getService()
	{
		return service;
	}
}
