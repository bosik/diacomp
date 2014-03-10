package org.bosik.diacomp.android.backend.features.diary;

import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Parser;
import org.bosik.diacomp.core.persistence.serializers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.serializers.utils.SerializerAdapter;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.utils.Utils;

public class WebDiaryService implements DiaryService
{
	// private static String TAG = WebDiaryService.class.getSimpleName();
	private final WebClient								webClient;
	private final Parser<DiaryRecord>					parser		= new ParserDiaryRecord();
	private final Parser<Versioned<DiaryRecord>>		parserV		= new ParserVersioned<DiaryRecord>(parser);
	private final Serializer<Versioned<DiaryRecord>>	serializerV	= new SerializerAdapter<Versioned<DiaryRecord>>(
																			parserV);

	/* ============================ CONSTRUCTOR ============================ */

	public WebDiaryService(WebClient webClient)
	{
		if (webClient == null)
		{
			throw new NullPointerException("WebClient can't be null");
		}

		this.webClient = webClient;
	}

	/* ============================ API ============================ */

	@Override
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException
	{
		try
		{
			String sGuids = Utils.formatJSONArray(guids);
			String query = "api/diary/guid/?guids=" + URLEncoder.encode(sGuids, "UTF-8");
			String s = webClient.doGetSmart(query, WebClient.CODEPAGE_UTF8);
			StdResponse resp = new StdResponse(s);
			WebClient.checkResponse(resp);

			return serializerV.readAll(resp.getResponse());
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date time, boolean includeRemoved) throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<DiaryRecord>> getRecords(Date fromDate, Date toDate, boolean includeRemoved)
			throws CommonServiceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		try
		{
			String query = "api/diary/";
			String items = serializerV.writeAll(records);

			List<NameValuePair> params = new ArrayList<NameValuePair>();
			params.add(new BasicNameValuePair("items", items));

			String s = webClient.doPutSmart(query, params, WebClient.CODEPAGE_UTF8);
			StdResponse resp = new StdResponse(s);
			WebClient.checkResponse(resp);
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	/* ======================= ROUTINES ========================= */

}
