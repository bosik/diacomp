package org.bosik.diacomp.web.backend.features.diary.service;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.web.backend.features.auth.service.AuthService;
import org.bosik.diacomp.web.backend.features.auth.service.FrontendAuthService;
import org.bosik.diacomp.web.backend.features.diary.function.DiaryDAO;
import org.bosik.diacomp.web.backend.features.diary.function.MySQLDiaryDAO;

public class FrontendDiaryService implements DiaryService
{
	private final AuthService	authService	= new FrontendAuthService();
	private final DiaryDAO		diaryDao	= new MySQLDiaryDAO();

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		int userId = authService.getCurrentUserId();
		diaryDao.delete(userId, id);
	}

	@Override
	public Versioned<DiaryRecord> findById(String id) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return diaryDao.findByGuid(userId, id);
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date since) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void save(List<Versioned<DiaryRecord>> items) throws CommonServiceException
	{
		// TODO Auto-generated method stub

	}

	@Override
	public List<Versioned<DiaryRecord>> findBetween(Date fromTime, Date toTime, boolean includeRemoved)
			throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

}