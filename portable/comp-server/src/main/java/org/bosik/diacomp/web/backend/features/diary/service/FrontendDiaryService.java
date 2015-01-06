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
		return diaryDao.findById(userId, id);
	}

	@Override
	public List<Versioned<DiaryRecord>> findByIdPrefix(String prefix) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return diaryDao.findByIdPrefix(userId, prefix);
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date since) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return diaryDao.findChanged(userId, since);
	}

	@Override
	public void save(List<Versioned<DiaryRecord>> items) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		diaryDao.post(userId, items);
	}

	@Override
	public List<Versioned<DiaryRecord>> findPeriod(Date startTime, Date endTime, boolean includeRemoved)
			throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return diaryDao.findPeriod(userId, startTime, endTime, includeRemoved);
	}
}
