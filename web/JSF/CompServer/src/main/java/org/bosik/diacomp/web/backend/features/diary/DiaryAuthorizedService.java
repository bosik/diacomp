package org.bosik.diacomp.web.backend.features.diary;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.bo.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.common.Versioned;
import org.bosik.diacomp.core.services.DiaryService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.auth.AuthRememberService;

public class DiaryAuthorizedService implements DiaryService
{
	private DiaryService		diaryService;
	private AuthRememberService	authService;

	public DiaryAuthorizedService(DiaryService diaryService, AuthRememberService authService)
	{
		if (authService == null)
		{
			throw new NullPointerException("Auth service can't be null");
		}
		if (diaryService == null)
		{
			throw new NullPointerException("Diary service can't be null");
		}
		this.authService = authService;
		this.diaryService = diaryService;
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException
	{
		try
		{
			return diaryService.getRecords(guids);
		}
		catch (NotAuthorizedException e)
		{
			authService.login();
			return diaryService.getRecords(guids);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date time, boolean includeRemoved) throws CommonServiceException
	{
		try
		{
			return diaryService.getRecords(time, includeRemoved);
		}
		catch (NotAuthorizedException e)
		{
			authService.login();
			return diaryService.getRecords(time, includeRemoved);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date fromTime, Date toTime, boolean includeRemoved)
			throws CommonServiceException
	{
		try
		{
			return diaryService.getRecords(fromTime, toTime, includeRemoved);
		}
		catch (NotAuthorizedException e)
		{
			authService.login();
			return diaryService.getRecords(fromTime, toTime, includeRemoved);
		}
	}

	@Override
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		try
		{
			diaryService.postRecords(records);
		}
		catch (NotAuthorizedException e)
		{
			authService.login();
			diaryService.postRecords(records);
		}
	}
}
