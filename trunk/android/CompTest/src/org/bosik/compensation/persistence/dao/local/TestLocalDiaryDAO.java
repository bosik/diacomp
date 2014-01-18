package org.bosik.compensation.persistence.dao.local;

import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.TestDiaryDAO;
import org.bosik.diacomp.persistence.dao.local.LocalDiaryDAO;
import android.content.ContentResolver;

public class TestLocalDiaryDAO extends TestDiaryDAO
{
	@Override
	protected DiaryDAO getDAO()
	{
		assertNotNull(getContext());
		ContentResolver resolver = getContext().getContentResolver();
		return new LocalDiaryDAO(resolver);
	}
}
