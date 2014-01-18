package org.bosik.compensation.persistence.dao;

import java.util.LinkedList;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.compensation.fakes.mocks.Mock;
import org.bosik.compensation.fakes.mocks.MockDiaryRecord;
import org.bosik.compensation.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.dao.DiaryDAO;

public abstract class TestDiaryDAO extends TestCase
{
	private DiaryDAO									diaryDAO;
	private static final Mock<DiaryRecord>				mockDiaryRecord				= new MockDiaryRecord();
	private static final Mock<Versioned<DiaryRecord>>	mockVersionedDiaryRecord	= new MockVersionedConverter<DiaryRecord>(
																							mockDiaryRecord);

	protected abstract DiaryDAO getDAO();

	@Override
	protected void setUp()
	{
		diaryDAO = getDAO();
	}

	public void testPersistanceMultiple()
	{
		List<Versioned<DiaryRecord>> org = mockVersionedDiaryRecord.getSamples();

		List<String> guids = new LinkedList<String>();
		for (Versioned<DiaryRecord> item : org)
		{
			guids.add(item.getId());
		}
		diaryDAO.postRecords(org);

		// ------------------
		setUp();

		List<Versioned<DiaryRecord>> restoredRecords = diaryDAO.getRecords(guids);
		assertEquals(org.size(), restoredRecords.size());

		// check content
		for (int i = 0; i < org.size(); i++)
		{
			final Versioned<DiaryRecord> exp = org.get(i);
			final Versioned<DiaryRecord> act = restoredRecords.get(i);
			mockVersionedDiaryRecord.compare(exp, act);
		}
	}

	public void testGetModList()
	{
		// fail("Not yet implemented"); // TODO
	}
}
