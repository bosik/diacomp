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
package org.bosik.diacomp.web.backend.features.base.food.set;

import org.bosik.diacomp.core.entities.business.FoodSetInfo;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodSetInfo;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.user.auth.UserRest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import java.util.List;

@Service
@Deprecated
@Path("food/set/")
public class FoodSetRest extends UserRest
{
	private static final String TYPE_JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=utf-8";

	@Autowired
	private FoodSetService foodSetService;

	private final Serializer<FoodSetInfo> serializerSetInfo = new SerializerAdapter<>(new ParserFoodSetInfo());

	@GET
	@Produces(TYPE_JSON_UTF8)
	public Response getFoodSetInfo()
	{
		try
		{
			List<FoodSetInfo> list = foodSetService.getFoodSetInfo();
			String response = serializerSetInfo.writeAll(list);
			return Response.ok(response).build();
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

	@GET
	@Path("{id}")
	@Produces(TYPE_JSON_UTF8)
	public Response getFoodSet(@PathParam("id") @DefaultValue("") String parId)
	{
		try
		{
			Utils.checkNotNull(parId, "ID expected (e.g. ../set/1ef0)");
			Utils.checkSize(parId, ObjectService.ID_FULL_SIZE);

			String data = foodSetService.getFoodSet(parId);

			if (data != null)
			{
				return Response.ok(data).build();
			}
			else
			{
				String response = String.format("Set %s not found", parId);
				return Response.status(Status.NOT_FOUND).entity(response).build();
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
