package org.bosik.diacomp.web.frontend.features.diary;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.frontend.features.auth.AuthRememberService;

public class DiaryAuthorizedService implements DiaryService
{
	private final DiaryService		diaryService;
	private final AuthRememberService	authService;

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
	public Versioned<DiaryRecord> getRecord(String guid) throws CommonServiceException
	{
		try
		{
			return diaryService.getRecord(guid);
		}
		catch (NotAuthorizedException e)
		{
			authService.login();
			return diaryService.getRecord(guid);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date time) throws CommonServiceException
	{
		try
		{
			return diaryService.getRecords(time);
		}
		catch (NotAuthorizedException e)
		{
			authService.login();
			return diaryService.getRecords(time);
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
