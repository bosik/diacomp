package org.bosik.diacomp.web.backend.features.diary;

import org.bosik.diacomp.core.test.fakes.services.FakeDiaryService;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("fake")
public class FakeDiarySpringService extends FakeDiaryService
{
	public FakeDiarySpringService()
	{
		super(true);
	}
}
