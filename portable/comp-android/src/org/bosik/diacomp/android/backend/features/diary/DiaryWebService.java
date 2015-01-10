package org.bosik.diacomp.android.backend.features.diary;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.utils.Utils;

public class DiaryWebService implements DiaryService
{
	// private static final String TAG = DiaryWebService.class.getSimpleName();

	// REST methods
	private static final String							API_DIARY_FIND_BY_ID		= "api/diary/guid/%s";
	private static final String							API_DIARY_FIND_BY_ID_PREFIX	= "api/diary/guid/%s";
	private static final String							API_DIARY_FIND_CHANGES		= "api/diary/changes/?since=%s";
	private static final String							API_DIARY_FIND_PERIOD		= "api/diary/period/?start_time=%s&end_time=%s&show_rem=%s";
	private static final String							API_DIARY_HASH				= "api/diary/hash/%s";
	private static final String							API_DIARY_HASHES			= "api/diary/hashes/%s";
	private static final String							API_DIARY_SAVE				= "api/diary/";

	private final WebClient								webClient;
	private final Parser<DiaryRecord>					parser						= new ParserDiaryRecord();
	private final Parser<Versioned<DiaryRecord>>		parserV						= new ParserVersioned<DiaryRecord>(
																							parser);
	private final Serializer<Versioned<DiaryRecord>>	serializerV					= new SerializerAdapter<Versioned<DiaryRecord>>(
																							parserV);
	private final Serializer<Map<String, String>>		serializerMap				= new SerializerMap();

	/* ============================ CONSTRUCTOR ============================ */

	public DiaryWebService(WebClient webClient)
	{
		if (webClient == null)
		{
			throw new NullPointerException("WebClient can't be null");
		}

		this.webClient = webClient;
	}

	/* ============================ API ============================ */

	@Override
	public Versioned<DiaryRecord> findById(String id) throws CommonServiceException
	{
		try
		{
			String query = String.format(API_DIARY_FIND_BY_ID, id);
			StdResponse resp = webClient.get(query);
			return serializerV.read(resp.getResponse());
		}
		catch (NotFoundException e)
		{
			return null;
		}
		catch (CommonServiceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(Date time) throws CommonServiceException
	{
		try
		{
			String query = String.format(API_DIARY_FIND_CHANGES, Utils.formatTimeUTC(time));
			StdResponse resp = webClient.get(query);
			return serializerV.readAll(resp.getResponse());
		}
		catch (CommonServiceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> findByIdPrefix(String prefix) throws CommonServiceException
	{
		try
		{
			String query = String.format(API_DIARY_FIND_BY_ID_PREFIX, prefix);
			StdResponse resp = webClient.get(query);
			return serializerV.readAll(resp.getResponse());
		}
		catch (CommonServiceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> findPeriod(Date startTime, Date endTime, boolean includeRemoved)
			throws CommonServiceException
	{
		try
		{
			String query = String.format(API_DIARY_FIND_PERIOD, Utils.formatTimeUTC(startTime),
					Utils.formatTimeUTC(endTime), Utils.formatBooleanStr(includeRemoved));
			StdResponse resp = webClient.get(query);
			return serializerV.readAll(resp.getResponse());
		}
		catch (CommonServiceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public String getHash(String prefix) throws CommonServiceException
	{
		try
		{
			String query = String.format(API_DIARY_HASH, prefix);
			StdResponse resp = webClient.get(query);
			return resp.getResponse();
		}
		catch (CommonServiceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public Map<String, String> getHashChildren(String prefix) throws CommonServiceException
	{
		try
		{
			String query = String.format(API_DIARY_HASHES, prefix);
			StdResponse resp = webClient.get(query);
			String data = resp.getResponse();
			return serializerMap.read(data);
		}
		catch (CommonServiceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void save(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	{
		try
		{
			String items = serializerV.writeAll(records);

			List<NameValuePair> params = new ArrayList<NameValuePair>();
			params.add(new BasicNameValuePair("items", items));

			webClient.put(API_DIARY_SAVE, params);
		}
		catch (CommonServiceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		Versioned<DiaryRecord> item = findById(id);

		if (item == null)
		{
			throw new NotFoundException(id);
		}
		if (item.isDeleted())
		{
			throw new AlreadyDeletedException(id);
		}

		item.setDeleted(true);
		save(Arrays.<Versioned<DiaryRecord>> asList(item));
	}

	@Override
	public void setHash(String prefix, String hash)
	{
		throw new UnsupportedOperationException("Can't set hash via REST");
	}
}
