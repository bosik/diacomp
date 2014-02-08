package org.bosik.diacomp.frontend.services;

import java.util.Date;
import java.util.List;
import javax.ws.rs.NotSupportedException;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.serializers.Serializer;
import org.bosik.diacomp.persistence.serializers.ready.SerializerDiaryRecord;
import org.bosik.diacomp.services.DiaryService;
import org.bosik.diacomp.services.exceptions.CommonServiceException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.utils.ResponseBuilder;
import org.bosik.diacomp.utils.StdResponse;
import org.bosik.diacomp.utils.Utils;
import org.json.JSONObject;

public class DiaryWebService implements DiaryService
{
	private static final String							BASE_URL	= "http://localhost:8080/CompServer/api/";
	private static Serializer<Versioned<DiaryRecord>>	serializer	= new SerializerDiaryRecord();

	@Override
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date time) throws CommonServiceException
	{
		Client client = ClientBuilder.newClient();
		WebTarget webTarget = client.target(BASE_URL + "diary/new");
		webTarget = webTarget.queryParam("mod_after", Utils.formatTimeUTC(time));
		String s = webTarget.request(MediaType.APPLICATION_JSON).get(String.class);

		StdResponse resp = new StdResponse(s);

		switch (resp.getCode())
		{
			case ResponseBuilder.CODE_OK:
			{
				JSONObject json = new JSONObject(s);
				String items = json.getString("items");
				return serializer.readAll(items);
			}
			case ResponseBuilder.CODE_UNAUTHORIZED:
				throw new NotAuthorizedException(resp.getMsg());
			default: // case ResponseBuilder.CODE_FAIL:
				throw new CommonServiceException(resp.getMsg());
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date fromDate, Date toDate) throws CommonServiceException
	{
		throw new NotSupportedException();
	}

	@Override
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		throw new NotSupportedException();
	}
}
