package org.bosik.diacomp.persistence.dao.fakes;

import org.bosik.diacomp.fakes.dao.FakeDiaryDAO;
import org.bosik.diacomp.persistence.dao.DiaryDAO;
import org.bosik.diacomp.persistence.dao.TestDiaryDAO;
import org.junit.Ignore;

@Ignore
public class TestFakeDiaryDAO extends TestDiaryDAO
{
	private static DiaryDAO	dao	= new FakeDiaryDAO();

	@Override
	protected DiaryDAO getDAO()
	{
		return dao;
	}
}
