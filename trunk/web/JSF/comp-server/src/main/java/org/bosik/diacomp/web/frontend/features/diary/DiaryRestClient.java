package org.bosik.diacomp.web.frontend.features.diary;

import java.util.Date;
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.ready.SerializerDiaryRecord;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.common.RestClient;
import com.sun.jersey.api.client.UniformInterfaceException;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.representation.Form;

public class DiaryRestClient extends RestClient implements DiaryService
{
	private static Serializer<Versioned<DiaryRecord>>	serializer	= new SerializerDiaryRecord();

	@Override
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException
	{
		try
		{
			WebResource resource = getResource("api/diary/guid");
			resource = resource.queryParam("guids", Utils.formatJSONArray(guids));
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
	public List<Versioned<DiaryRecord>> getRecords(Date time, boolean includeRemoved) throws CommonServiceException
	{
		try
		{
			WebResource resource = getResource("api/diary/new");
			resource = resource.queryParam("mod_after", Utils.formatTimeUTC(time));
			resource = resource.queryParam("show_rem", Utils.formatBooleanStr(includeRemoved));
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
	public List<Versioned<DiaryRecord>> getRecords(Date fromTime, Date toTime, boolean includeRemoved)
			throws CommonServiceException
	{
		try
		{
			WebResource resource = getResource("api/diary/period");
			resource = resource.queryParam("start_time", Utils.formatTimeUTC(fromTime));
			resource = resource.queryParam("end_time", Utils.formatTimeUTC(toTime));
			resource = resource.queryParam("show_rem", Utils.formatBooleanStr(includeRemoved));
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
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		WebResource resource = getResource("api/diary/");
		try
		{
			Form form = new Form();
			form.add("items", serializer.writeAll(records));
			String str = resource.accept(MediaType.APPLICATION_JSON).put(String.class, form);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);
		}
		catch (UniformInterfaceException e)
		{
			System.err.println(e.getResponse().getEntity(String.class));
			throw new CommonServiceException("URL: " + resource.getURI(), e);
		}
	}
}
