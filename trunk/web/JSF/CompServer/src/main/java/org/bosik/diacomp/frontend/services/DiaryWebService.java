package org.bosik.diacomp.frontend.services;

import java.util.Date;
import java.util.List;

import javax.ws.rs.NotSupportedException;
import javax.ws.rs.core.MediaType;

import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.serializers.Serializer;
import org.bosik.diacomp.persistence.serializers.ready.SerializerDiaryRecord;
import org.bosik.diacomp.services.DiaryService;
import org.bosik.diacomp.services.exceptions.CommonServiceException;
import org.bosik.diacomp.utils.StdResponse;
import org.bosik.diacomp.utils.Utils;

import com.sun.jersey.api.client.UniformInterfaceException;
import com.sun.jersey.api.client.WebResource;

public class DiaryWebService extends WebService implements DiaryService
{
	private static Serializer<Versioned<DiaryRecord>>	serializer	= new SerializerDiaryRecord();

	@Override
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date time, boolean includeRemoved) throws CommonServiceException
	{
		try
		{
			WebResource resource = getResource("api/diary/new");
			resource = resource.queryParam("mod_after", Utils.formatTimeUTC(time));
			String str = resource.accept(MediaType.APPLICATION_JSON).get(String.class);

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
	public List<Versioned<DiaryRecord>> getRecords(Date fromDate, Date toDate, boolean includeRemoved)
			throws CommonServiceException
	{
		throw new NotSupportedException();
	}

	@Override
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		throw new NotSupportedException();
	}
}
