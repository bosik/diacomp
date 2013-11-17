package org.bosik.compensation.persistence.repository.diary;

import android.content.ContentResolver;

public class LocalDiaryRepositoryTest extends DiaryRepositoryTest
{
	@Override
	protected DiaryRepository getRepository()
	{
		ContentResolver resolver = getContext().getContentResolver();
		return new LocalDiaryRepository(resolver);
	}
}
