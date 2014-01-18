package org.bosik.compensation.persistence.dao.local;

import org.bosik.compensation.persistence.dao.TestDiaryDAO;
import org.bosik.diacomp.persistence.dao.DiaryDAO;
import org.bosik.diacomp.persistence.dao.local.LocalDiaryDAO;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class TestLocalDiaryDAO extends AndroidTestCase
{
	private TestDiaryDAO	tester	= new TestDiaryDAO()
									{
										@Override
										protected DiaryDAO getDAO()
										{
											AndroidTestCase a = new AndroidTestCase();

											assertNotNull(a.getContext());
											ContentResolver resolver = getContext().getContentResolver();
											return new LocalDiaryDAO(resolver);
										}
									};

	public void testPersistanceMultiple()
	{
		tester.testPersistanceMultiple();
	}
}
