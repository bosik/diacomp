package org.bosik.diacomp.android.backend.features.dishbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDishItem;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.sync.MerkleTree;
import org.bosik.diacomp.core.utils.Utils;

@SuppressWarnings("unchecked")
public class DishBaseWebService implements DishBaseService
{
	// private static final String TAG = DishBaseWebService.class.getSimpleName();

	// REST methods
	private static final String						API_DISH_COUNT				= "api/dish/count/%s";
	private static final String						API_DISH_FIND_ALL			= "api/dish/all/?show_rem=%s";
	private static final String						API_DISH_FIND_ANY			= "api/dish/search/?q=%s";
	private static final String						API_DISH_FIND_BY_ID			= "api/dish/guid/%s";
	private static final String						API_DISH_FIND_BY_ID_PREFIX	= "api/dish/guid/%s";
	private static final String						API_DISH_FIND_CHANGES		= "api/dish/changes/?since=%s";
	private static final String						API_DISH_HASH				= "api/dish/hash/%s";
	private static final String						API_DISH_HASHES				= "api/dish/hashes/%s";
	private static final String						API_DISH_SAVE				= "api/dish/";

	private final WebClient							webClient;
	private final Serializer<Versioned<DishItem>>	serializer					= new SerializerDishItem();
	private final Serializer<Map<String, String>>	serializerMap				= new SerializerMap();

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
	public int count(String prefix)
	{
		try
		{
			String query = String.format(API_DISH_COUNT, prefix);
			StdResponse resp = webClient.get(query);
			return Integer.parseInt(resp.getResponse());
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
		save(Arrays.<Versioned<DishItem>> asList(item));
	}

	@Override
	public List<Versioned<DishItem>> findAll(boolean includeRemoved)
	{
		try
		{
			String url = String.format(API_DISH_FIND_ALL, Utils.formatBooleanInt(includeRemoved));
			StdResponse resp = webClient.get(url);
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
			String url = String.format(API_DISH_FIND_ANY, filter);
			StdResponse resp = webClient.get(url);
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
			String url = String.format(API_DISH_FIND_CHANGES, Utils.formatTimeUTC(since));
			StdResponse resp = webClient.get(url);
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
	public Versioned<DishItem> findById(String id)
	{
		try
		{
			String url = String.format(API_DISH_FIND_BY_ID, id);
			StdResponse resp = webClient.get(url);
			return serializer.read(resp.getResponse());
		}
		catch (NotFoundException e)
		{
			return null;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<DishItem>> findByIdPrefix(String prefix) throws CommonServiceException
	{
		try
		{
			String url = String.format(API_DISH_FIND_BY_ID_PREFIX, prefix);
			StdResponse resp = webClient.get(url);
			return serializer.readAll(resp.getResponse());
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
			String query = String.format(API_DISH_HASH, prefix);
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
			String query = String.format(API_DISH_HASHES, prefix);
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
	public MerkleTree getHashTree()
	{
		return new MerkleTree()
		{
			@Override
			public String getHash(String prefix)
			{
				return DishBaseWebService.this.getHash(prefix);
			}

			@Override
			public Map<String, String> getHashChildren(String prefix)
			{
				return DishBaseWebService.this.getHashChildren(prefix);
			}
		};
	}

	@Override
	public void save(List<Versioned<DishItem>> items) throws NotFoundException, PersistenceException
	{
		if (items.isEmpty())
		{
			return;
		}

		try
		{
			List<NameValuePair> params = new ArrayList<NameValuePair>();
			params.add(new BasicNameValuePair("items", serializer.writeAll(items)));
			webClient.put(API_DISH_SAVE, params);
		}
		catch (Exception e)
		{
			throw new CommonServiceException("URL: " + API_DISH_SAVE, e);
		}
	}

	@Override
	public void setHash(String prefix, String hash)
	{
		throw new UnsupportedOperationException("Can't set hash via REST");
	}
}
