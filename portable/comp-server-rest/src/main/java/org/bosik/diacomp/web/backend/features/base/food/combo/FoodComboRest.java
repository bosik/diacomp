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
package org.bosik.diacomp.web.backend.features.base.food.combo;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodComboLocalService;
import org.bosik.diacomp.web.backend.features.user.auth.UserRest;
import org.bosik.merklesync.DataSource;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * Legacy service, mostly for Windows clients
 */
@Service
@Path("food/")
public class FoodBaseRest extends UserRest
{
	private static final String TYPE_JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=utf-8";

	@Autowired
	private FoodComboLocalService foodComboService;

	private final Serializer<Versioned<FoodItem>> serializer    = new SerializerFoodItem();
	private final Serializer<Map<String, String>> serializerMap = new SerializerMap();

	@GET
	@Path("count/{prefix: .*}")
	@Produces(TYPE_JSON_UTF8)
	public Response count(@PathParam("prefix") @DefaultValue("") String parPrefix)
	{
		try
		{
			Utils.checkNotNull(parPrefix, "ID prefix expected (e.g. ../count/1ef0)");
			Utils.checkSize(parPrefix, ObjectService.ID_FULL_SIZE);

			int count = foodComboService.count(getUserId(), parPrefix);
			String response = String.valueOf(count);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
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
	@Path("guid/{guid: .*}")
	@Produces(TYPE_JSON_UTF8)
	public Response findById(@PathParam("guid") String parId)
	{
		try
		{
			Utils.checkNotNull(parId, "ID expected (e.g. ../guid/1ef0)");
			Utils.checkSize(parId, ObjectService.ID_FULL_SIZE);

			// Prefix form
			if (parId.length() <= DataSource.ID_PREFIX_SIZE)
			{
				List<Versioned<FoodItem>> items = foodComboService.findByIdPrefix(getUserId(), parId);

				String response = serializer.writeAll(items);
				return Response.ok(response).build();
			}
			else
			// Full form
			{
				Versioned<FoodItem> item = foodComboService.findById(getUserId(), parId);

				if (item != null)
				{
					String response = serializer.write(item);
					return Response.ok(response).build();
				}
				else
				{
					String response = String.format("Item %s not found", parId);
					return Response.status(Status.NOT_FOUND).entity(response).build();
				}
			}
		}
		catch (TooManyItemsException e)
		{
			return Response.status(Status.BAD_REQUEST).entity("Too many items found").build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
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
	@Path("all")
	@Produces(TYPE_JSON_UTF8)
	public Response findAll(@QueryParam("show_rem") @DefaultValue("false") String parShowRem)
	{
		try
		{
			Utils.checkSize(parShowRem, 5); // "false".length

			boolean includeRemoved = Boolean.valueOf(parShowRem);

			List<Versioned<FoodItem>> items = foodComboService.findAll(getUserId(), includeRemoved);
			String response = serializer.writeAll(items);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
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
	@Path("search")
	@Produces(TYPE_JSON_UTF8)
	public Response findAny(@QueryParam("q") String filter)
	{
		try
		{
			Utils.checkNotNull(filter, "Missing parameter: q");
			Utils.checkSize(filter, 256);

			List<Versioned<FoodItem>> items = foodComboService.findAny(getUserId(), filter);
			String response = serializer.writeAll(items);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
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
	@Path("changes")
	@Produces(TYPE_JSON_UTF8)
	public Response findChanged(@QueryParam("since") String parTime)
	{
		try
		{
			Utils.checkNotNull(parTime, "Missing parameter: since");
			Utils.checkSize(parTime, Utils.FORMAT_DATE_TIME.length());

			Date since = Utils.parseTimeUTC(parTime);
			List<Versioned<FoodItem>> items = foodComboService.findChanged(getUserId(), since);
			String response = serializer.writeAll(items);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
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
	@Path("hash/{prefix: .*}")
	@Produces(TYPE_JSON_UTF8)
	public Response getHash(@PathParam("prefix") @DefaultValue("") String parPrefix)
	{
		try
		{
			Utils.checkNotNull(parPrefix, "ID prefix expected (e.g. ../hash/1ef0)");
			Utils.checkSize(parPrefix, ObjectService.ID_FULL_SIZE);

			MerkleTree hashTree = foodComboService.getHashTree(getUserId());
			String s = hashTree.getHash(parPrefix);
			return Response.ok(s != null ? s : "").build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
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
	@Path("hashes/{prefix: .*}")
	@Produces(TYPE_JSON_UTF8)
	public Response getHashChildren(@PathParam("prefix") @DefaultValue("") String parPrefix)
	{
		try
		{
			Utils.checkNotNull(parPrefix, "ID prefix expected (e.g. ../hashes/1ef0)");
			Utils.checkSize(parPrefix, ObjectService.ID_FULL_SIZE);

			MerkleTree hashTree = foodComboService.getHashTree(getUserId());
			Map<String, String> map = hashTree.getHashChildren(parPrefix);
			String response = serializerMap.write(map);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
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

	@PUT
	@Produces(TYPE_JSON_UTF8)
	public Response save(@FormParam("items") String parItems)
	{
		try
		{
			Utils.checkNotNull(parItems, "Missing parameter: items");

			List<Versioned<FoodItem>> items = serializer.readAll(Utils.removeNonUtf8(parItems));
			foodComboService.save(getUserId(), items);

			return Response.ok("Saved OK").build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
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
