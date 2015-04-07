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
package org.bosik.diacomp.android.backend.features.foodset;

import java.util.List;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.FoodSetInfo;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodSetInfo;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.merklesync.Versioned;

public class FoodSetService
{
	// REST methods
	private static final String						API_FOODSET_ALL		= "api/food/set/";
	private static final String						API_FOODSET_ID		= "api/food/set/%s";

	private final WebClient							webClient;
	private final Serializer<Versioned<FoodItem>>	serializer			= new SerializerFoodItem();
	private final Serializer<FoodSetInfo>			serializerSetInfo	= new SerializerAdapter<FoodSetInfo>(
																				new ParserFoodSetInfo());

	public FoodSetService(WebClient webClient)
	{
		this.webClient = webClient;
	}

	public List<FoodSetInfo> getFoodSets()
	{
		try
		{
			String query = API_FOODSET_ALL;
			String resp = webClient.get(query);
			return serializerSetInfo.readAll(resp);
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

	public List<Versioned<FoodItem>> getFoodSet(String id)
	{
		try
		{
			String query = String.format(API_FOODSET_ID, id);
			String resp = webClient.get(query);
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
}
