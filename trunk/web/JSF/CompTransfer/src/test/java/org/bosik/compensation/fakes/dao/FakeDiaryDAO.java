package org.bosik.compensation.fakes.dao;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.dao.DiaryDAO;
import org.bosik.diacomp.persistence.exceptions.CommonDAOException;

public class FakeDiaryDAO implements DiaryDAO
{
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonDAOException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Versioned<DiaryRecord>> getRecords(Date time) throws CommonDAOException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List<Versioned<DiaryRecord>> getRecords(Date fromDate, Date toDate) throws CommonDAOException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonDAOException
	{
		// TODO Auto-generated method stub

	}
}
