package org.bosik.compensation.persistence.repository.diary;

import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.local.LocalDiaryDAO;
import android.content.ContentResolver;

public class LocalDiaryRepositoryTest extends DiaryRepositoryTest
{
	@Override
	protected DiaryDAO getRepository()
	{
		ContentResolver resolver = getContext().getContentResolver();
		return new LocalDiaryDAO(resolver);
	}
}
