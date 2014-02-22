package org.bosik.diacomp.fakes.services;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.bo.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.common.Versioned;
import org.bosik.diacomp.core.services.DiaryService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;

public class FakeDiaryService implements DiaryService
{
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Versioned<DiaryRecord>> getRecords(Date time, boolean includeRemoved) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Versioned<DiaryRecord>> getRecords(Date fromDate, Date toDate, boolean includeRemoved)
			throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		// TODO Auto-generated method stub
	}
}
