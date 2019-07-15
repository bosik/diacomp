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

import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.services.base.food.FoodCommonService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.Date;
import java.util.List;

@SuppressWarnings("unchecked")
public class FoodCommonWebService implements FoodCommonService
{
	// REST methods
	private static final String API_FOOD_COMMON_FIND_ALL     = "api/food/common/";
	private static final String API_FOOD_COMMON_FIND_CHANGED = "api/food/common/?lastModified=%s";

	private final WebClient                       webClient;
	private final Serializer<Versioned<FoodItem>> serializer = new SerializerFoodItem();

	public FoodCommonWebService(WebClient webClient)
	{
		this.webClient = webClient;
	}

	@Override
	public List<Versioned<FoodItem>> findAll()
	{
		String url = API_FOOD_COMMON_FIND_ALL;
		String resp = webClient.get(url);
		return serializer.readAll(resp);
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(Date since)
	{
		String url = String.format(API_FOOD_COMMON_FIND_CHANGED, Utils.formatTimeUTC(since));
		String resp = webClient.get(url);
		return serializer.readAll(resp);
	}
}
