package org.bosik.diacomp.fakes.services;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.exceptions.CommonServiceException;
import services.DiaryService;

public class FakeDiaryService implements DiaryService
{
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Versioned<DiaryRecord>> getRecords(Date time) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Versioned<DiaryRecord>> getRecords(Date fromDate, Date toDate) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		// TODO Auto-generated method stub

	}
}
