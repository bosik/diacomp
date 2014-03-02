package org.bosik.diacomp.core.persistence.services;

import java.util.LinkedList;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.DiaryService;
import org.bosik.diacomp.core.testutils.fakes.mocks.Mock;
import org.bosik.diacomp.core.testutils.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.testutils.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.core.testutils.fakes.mocks.VersionedUtils;

public abstract class TestDiaryService extends TestCase
{
	private DiaryService								diaryService;
	private static final Mock<DiaryRecord>				mockDiaryRecord				= new MockDiaryRecord();
	private static final Mock<Versioned<DiaryRecord>>	mockVersionedDiaryRecord	= new MockVersionedConverter<DiaryRecord>(
																							mockDiaryRecord);

	protected abstract DiaryService getService();

	@Override
	protected void setUp()
	{
		diaryService = getService();
	}

	public void testPersistanceMultiple()
	{
		List<Versioned<DiaryRecord>> org = mockVersionedDiaryRecord.getSamples();
		VersionedUtils.enumerate(org);

		List<String> guids = new LinkedList<String>();
		for (Versioned<DiaryRecord> item : org)
		{
			guids.add(item.getId());
		}
		diaryService.postRecords(org);

		// ------------------
		setUp();

		List<Versioned<DiaryRecord>> restoredRecords = diaryService.getRecords(guids);
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
