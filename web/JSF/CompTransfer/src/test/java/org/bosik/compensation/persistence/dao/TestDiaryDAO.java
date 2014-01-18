package org.bosik.compensation.persistence.dao;

import java.util.LinkedList;
import java.util.List;
import org.bosik.compensation.fakes.mocks.Mock;
import org.bosik.compensation.fakes.mocks.MockDiaryRecord;
import org.bosik.compensation.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.dao.DiaryDAO;
import org.junit.Assert;

public abstract class TestDiaryDAO
{
	private DiaryDAO									diaryDAO_cached;
	private static final Mock<DiaryRecord>				mockDiaryRecord				= new MockDiaryRecord();
	private static final Mock<Versioned<DiaryRecord>>	mockVersionedDiaryRecord	= new MockVersionedConverter<DiaryRecord>(
																							mockDiaryRecord);

	protected abstract DiaryDAO getDAO();

	private DiaryDAO getDiaryDAO()
	{
		if (diaryDAO_cached == null)
		{
			diaryDAO_cached = getDAO();
		}
		return diaryDAO_cached;
	}

	public void testPersistanceMultiple()
	{
		List<Versioned<DiaryRecord>> org = mockVersionedDiaryRecord.getSamples();

		List<String> guids = new LinkedList<String>();
		for (Versioned<DiaryRecord> item : org)
		{
			guids.add(item.getId());
		}
		getDiaryDAO().postRecords(org);

		// ------------------
		diaryDAO_cached = null;

		List<Versioned<DiaryRecord>> restoredRecords = getDiaryDAO().getRecords(guids);
		Assert.assertEquals(org.size(), restoredRecords.size());

		// check content
		for (int i = 0; i < org.size(); i++)
		{
			final Versioned<DiaryRecord> exp = org.get(i);
			final Versioned<DiaryRecord> act = restoredRecords.get(i);
			mockVersionedDiaryRecord.compare(exp, act);
		}
	}
}
