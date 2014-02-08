package org.bosik.diacomp.frontend.services;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.backend.services.DiaryWebServiceImpl;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.services.DiaryService;
import org.bosik.diacomp.services.exceptions.CommonServiceException;

public class DiaryWebService implements DiaryService
{
	private DiaryWebServiceImpl	dao	= new DiaryWebServiceImpl();

	@Override
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date time) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date fromDate, Date toDate) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		// TODO Auto-generated method stub

	}
}
