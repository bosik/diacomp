package org.bosik.diacomp.web.frontend.features.diary;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.ready.SerializerDiaryRecord;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.common.AuthorizedRestClient;
import com.sun.jersey.api.client.UniformInterfaceException;
import com.sun.jersey.api.representation.Form;

public class DiaryRestClient extends AuthorizedRestClient implements DiaryService
{
	public DiaryRestClient(AuthService authService, String login, String pass, int apiVersion)
	{
		super(authService, login, pass, apiVersion);
	}

	private static Serializer<Versioned<DiaryRecord>>	serializer	= new SerializerDiaryRecord();

	@Override
	public Versioned<DiaryRecord> getRecord(String guid) throws CommonServiceException
	{
		try
		{
			String str = authGet(String.format("api/diary/guid/%s", guid));

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			Versioned<DiaryRecord> item = serializer.read(resp.getResponse());
			return item;
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date time) throws CommonServiceException
	{
		try
		{
			String str = authGet(String.format("api/diary/changes/?since=%s", Utils.formatTimeUTC(time)));
			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date fromTime, Date toTime, boolean includeRemoved)
			throws CommonServiceException
	{
		try
		{
			String url = "api/diary/period/?";
			url += "start_time=" + Utils.formatTimeUTC(fromTime);
			url += "&end_time=" + Utils.formatTimeUTC(toTime);
			url += "&show_rem=" + Utils.formatBooleanStr(includeRemoved);

			StdResponse resp = new StdResponse(url);
			checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		String url = "api/diary/";
		try
		{
			Form form = new Form();
			form.add("items", serializer.writeAll(records));
			String str = authPut(url, form);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);
		}
		catch (UniformInterfaceException e)
		{
			System.err.println(e.getResponse().getEntity(String.class));
			throw new CommonServiceException("URL: " + url, e);
		}
	}
}
