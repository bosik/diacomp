package org.bosik.compensation.persistence.dao.fakes;

import org.bosik.compensation.fakes.dao.FakeDiaryDAO;
import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.DiaryDAOTest;

public class FakeDiaryDAOTest extends DiaryDAOTest
{
	@Override
	protected DiaryDAO getDAO()
	{
		return new FakeDiaryDAO();
	}
}
