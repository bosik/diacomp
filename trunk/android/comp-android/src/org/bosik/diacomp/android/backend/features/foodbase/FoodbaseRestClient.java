package org.bosik.diacomp.android.backend.features.foodbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.ready.SerializerFoodItem;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.core.utils.Utils;

public class FoodbaseRestClient implements FoodBaseService
{
	// private static final String TAG = FoodbaseRestClient.class.getSimpleName();

	private final WebClient							webClient;
	private final Serializer<Versioned<FoodItem>>	serializer	= new SerializerFoodItem();

	public FoodbaseRestClient(WebClient webClient)
	{
		this.webClient = webClient;
	}

	@Override
	public String add(Versioned<FoodItem> item) throws PersistenceException
	{
		item.setId(Utils.generateGuid());
		save(Arrays.asList(item));
		return item.getId();
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		Versioned<FoodItem> item = findById(id);

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
	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		try
		{
			String url = String.format("api/food/all/?show_rem=%s", Utils.formatBooleanInt(includeRemoved));
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
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findById(String guid)
	{
		try
		{
			String url = String.format("api/food/guid/%s", guid);
			String str = webClient.doGetSmart(url);

			StdResponse resp = new StdResponse(str);
			WebClient.checkResponse(resp);

			Versioned<FoodItem> item = !resp.getResponse().isEmpty() ? serializer.read(resp.getResponse()) : null;
			return item;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void save(List<Versioned<FoodItem>> items) throws NotFoundException, PersistenceException
	{
		String url = "api/food/";
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
			System.err.println(str);
			throw new CommonServiceException("URL: " + url, e);
		}
	}
}
