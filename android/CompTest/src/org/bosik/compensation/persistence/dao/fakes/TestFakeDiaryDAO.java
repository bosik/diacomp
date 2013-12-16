package org.bosik.compensation.persistence.dao.fakes;

import org.bosik.compensation.fakes.dao.FakeDiaryDAO;
import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.TestDiaryDAO;
import android.test.suitebuilder.annotation.Suppress;

@Suppress
public class TestFakeDiaryDAO extends TestDiaryDAO
{
	private static DiaryDAO	dao	= new FakeDiaryDAO();

	@Override
	protected DiaryDAO getDAO()
	{
		return dao;
	}
}
