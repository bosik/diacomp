/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.backend.features.foodbase;

import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

@SuppressWarnings("unchecked")
public class FoodBaseWebService implements FoodBaseService
{
	// private static final String TAG = FoodBaseWebService.class.getSimpleName();

	// REST methods
	private static final String API_FOOD_COUNT             = "api/food/count/%s";
	private static final String API_FOOD_FIND_ALL          = "api/food/all/?show_rem=%s";
	private static final String API_FOOD_FIND_ANY          = "api/food/search/?q=%s";
	private static final String API_FOOD_FIND_BY_ID        = "api/food/guid/%s";
	private static final String API_FOOD_FIND_BY_ID_PREFIX = "api/food/guid/%s";
	private static final String API_FOOD_FIND_CHANGES      = "api/food/changes/?since=%s";
	private static final String API_FOOD_HASH              = "api/food/hash/%s";
	private static final String API_FOOD_HASHES            = "api/food/hashes/%s";
	private static final String API_FOOD_SAVE              = "api/food/";

	private final WebClient webClient;
	private final Serializer<Versioned<FoodItem>> serializer    = new SerializerFoodItem();
	private final Serializer<Map<String, String>> serializerMap = new SerializerMap();

	public FoodBaseWebService(WebClient webClient)
	{
		this.webClient = webClient;
	}

	@Override
	public void add(Versioned<FoodItem> item) throws PersistenceException
	{
		// TODO: current implementation doesn't fail for duplicates
		save(Collections.singletonList(item));
	}

	@Override
	public int count(String prefix)
	{
		try
		{
			String query = String.format(API_FOOD_COUNT, prefix);
			String resp = webClient.get(query);
			return Integer.parseInt(resp);
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
		item.modified();
		save(Collections.singletonList(item));
	}

	@Override
	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		try
		{
			String url = String.format(API_FOOD_FIND_ALL, Utils.formatBooleanInt(includeRemoved));
			String resp = webClient.get(url);
			return serializer.readAll(resp);
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		try
		{
			String url = String.format(API_FOOD_FIND_ANY, filter);
			String resp = webClient.get(url);
			return serializer.readAll(resp);
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(Date since)
	{
		try
		{
			String url = String.format(API_FOOD_FIND_CHANGES, Utils.formatTimeUTC(since));
			String resp = webClient.get(url);
			return serializer.readAll(resp);
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findById(String id)
	{
		try
		{
			String url = String.format(API_FOOD_FIND_BY_ID, id);
			String resp = webClient.get(url);
			return serializer.read(resp);
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
	public List<Versioned<FoodItem>> findByIdPrefix(String prefix) throws CommonServiceException
	{
		try
		{
			String url = String.format(API_FOOD_FIND_BY_ID_PREFIX, prefix);
			String resp = webClient.get(url);
			return serializer.readAll(resp);
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
				try
				{
					String query = String.format(API_FOOD_HASH, prefix);
					return webClient.get(query);
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
			public Map<String, String> getHashChildren(String prefix)
			{
				try
				{
					String query = String.format(API_FOOD_HASHES, prefix);
					String resp = webClient.get(query);
					return serializerMap.read(resp);
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
		};
	}

	@Override
	public void save(List<Versioned<FoodItem>> items) throws PersistenceException
	{
		if (items.isEmpty())
		{
			return;
		}

		try
		{
			List<NameValuePair> params = new ArrayList<NameValuePair>();
			params.add(new BasicNameValuePair("items", serializer.writeAll(items)));
			webClient.put(API_FOOD_SAVE, params);
		}
		catch (Exception e)
		{
			throw new CommonServiceException("URL: " + API_FOOD_SAVE, e);
		}
	}
}
