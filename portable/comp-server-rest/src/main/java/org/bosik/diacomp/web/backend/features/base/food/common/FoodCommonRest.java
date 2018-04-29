/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.base.food.common;

import org.bosik.diacomp.core.entities.business.foodbase.FoodCommon;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodCommon;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.utils.Utils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import java.util.Date;
import java.util.List;

@Service
@Path("food/common/")
public class FoodCommonRest
{
	private static final String TYPE_JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=utf-8";

	@Autowired
	private FoodCommonLocalService foodCommonService;

	//	@Autowired
	//	private FoodSetService foodSetService;

	//	private final Serializer<Versioned<FoodItem>> serializerFoodItem   = new SerializerFoodItem();
	private final Serializer<FoodCommon> serializerFoodCommon = new SerializerFoodCommon();

	//	@GET
	//	@Path("init")
	//	@Produces(TYPE_JSON_UTF8)
	//	public Response prepare()
	//	{
	//		try
	//		{
	//			List<FoodSetInfo> sets = foodSetService.getFoodSetInfo();
	//			for (FoodSetInfo set : sets)
	//			{
	//				String data = foodSetService.getFoodSet(set.getId());
	//				List<Versioned<FoodItem>> items = serializerFoodItem.readAll(data);
	//
	//				foodCommonService.upload(set.getDescription(), items);
	//			}
	//
	//			return Response.ok("Uploaded OK").build();
	//		}
	//		catch (IllegalArgumentException e)
	//		{
	//			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
	//		}
	//		catch (Exception e)
	//		{
	//			e.printStackTrace();
	//			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
	//		}
	//	}

	@GET
	@Produces(TYPE_JSON_UTF8)
	public Response find(@QueryParam("lastModified") String parTime)
	{
		try
		{
			if (parTime != null)
			{
				Utils.checkSize(parTime, Utils.FORMAT_DATE_TIME.length());
				Date lastModified = Utils.parseTimeUTC(parTime);

				List<FoodCommon> foods = foodCommonService.find(lastModified);
				String data = serializerFoodCommon.writeAll(foods);
				return Response.ok(data).build();
			}
			else
			{
				List<FoodCommon> foods = foodCommonService.find();
				String data = serializerFoodCommon.writeAll(foods);
				return Response.ok(data).build();
			}
		}
		catch (IllegalArgumentException e)
		{
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}
}
