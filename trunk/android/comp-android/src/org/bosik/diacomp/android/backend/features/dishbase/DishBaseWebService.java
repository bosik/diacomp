package org.bosik.diacomp.android.backend.features.dishbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDishItem;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.dishbase.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.utils.Utils;

public class DishBaseWebService implements DishBaseService
{
	// private static final String TAG = DishBaseWebService.class.getSimpleName();

	private final WebClient							webClient;
	private final Serializer<Versioned<DishItem>>	serializer	= new SerializerDishItem();

	public DishBaseWebService(WebClient webClient)
	{
		this.webClient = webClient;
	}

	@Override
	public void add(Versioned<DishItem> item) throws PersistenceException
	{
		// TODO: current implementation doesn't fail for duplicates
		save(Arrays.<Versioned<DishItem>> asList(item));
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		Versioned<DishItem> item = findById(id);

		if (item == null)
		{
			throw new NotFoundException(id);
		}

		if (item.isDeleted())
		{
			throw new AlreadyDeletedException(id);
		}

		item.setDeleted(true);
		save(Arrays.asList(item));
	}

	@Override
	public List<Versioned<DishItem>> findAll(boolean includeRemoved)
	{
		try
		{
			String url = String.format("api/dish/all/?show_rem=%s", Utils.formatBooleanInt(includeRemoved));
			String str = webClient.doGetSmart(url);

			StdResponse resp = new StdResponse(str);
			WebClient.checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<DishItem>> findAny(String filter)
	{
		try
		{
			String url = String.format("api/dish/search/?q=%s", filter);
			String str = webClient.doGetSmart(url);

			StdResponse resp = new StdResponse(str);
			WebClient.checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<DishItem>> findChanged(Date since)
	{
		try
		{
			String url = String.format("api/dish/changes/?since=%s", Utils.formatTimeUTC(since));
			String str = webClient.doGetSmart(url);

			StdResponse resp = new StdResponse(str);
			WebClient.checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public Versioned<DishItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<DishItem> findById(String guid)
	{
		try
		{
			String url = String.format("api/dish/guid/%s", guid);
			String str = webClient.doGetSmart(url);

			StdResponse resp = new StdResponse(str);
			WebClient.checkResponse(resp);

			Versioned<DishItem> item = !resp.getResponse().isEmpty() ? serializer.read(resp.getResponse()) : null;
			return item;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void save(List<Versioned<DishItem>> items) throws NotFoundException, PersistenceException
	{
		String url = "api/dish/";
		String str = null;
		try
		{
			List<NameValuePair> params = new ArrayList<NameValuePair>();
			params.add(new BasicNameValuePair("items", serializer.writeAll(items)));
			str = webClient.doPutSmart(url, params, WebClient.CODEPAGE_UTF8);

			StdResponse resp = new StdResponse(str);
			WebClient.checkResponse(resp);
		}
		catch (Exception e)
		{
			throw new CommonServiceException("URL: " + url, e);
		}
	}
}
