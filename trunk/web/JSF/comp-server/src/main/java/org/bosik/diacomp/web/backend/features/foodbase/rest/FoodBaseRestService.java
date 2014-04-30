package org.bosik.diacomp.web.backend.features.foodbase.rest;

import java.util.Date;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.UserSessionUtils;
import org.bosik.diacomp.web.backend.features.foodbase.function.FakeFoodbaseDAO;
import org.bosik.diacomp.web.backend.features.foodbase.function.FoodbaseDAO;

@Path("food/")
public class FoodBaseRestService
{
	@Context
	HttpServletRequest										req;

	private final FoodbaseDAO								foodbaseService	= new FakeFoodbaseDAO();

	private static final Serializer<Versioned<FoodItem>>	serializer	= new SerializerFoodItem();

	@GET
	@Path("guid/{guid}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecordsGuid(@PathParam("guid") String parGuid) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);
			Versioned<FoodItem> item = foodbaseService.findById(userId, parGuid);

			if (item != null)
			{
				String s = serializer.write(item);
				String response = ResponseBuilder.buildDone(s);
				return Response.ok(response).build();
			}
			else
			{
				String response = ResponseBuilder.build(ResponseBuilder.CODE_NOTFOUND,
						String.format("Item %s not found", parGuid));
				return Response.ok(response).build();
			}

		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("all")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecordsAll(@QueryParam("show_rem") String parShowRem) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);
			boolean includeRemoved = Boolean.valueOf(parShowRem);

			List<Versioned<FoodItem>> items = foodbaseService.findAll(userId, includeRemoved);
			String s = serializer.writeAll(items);
			String response = ResponseBuilder.buildDone(s);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("search")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecordsFilter(@QueryParam("q") String filter) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);

			List<Versioned<FoodItem>> items = foodbaseService.findAny(userId, filter);
			String s = serializer.writeAll(items);
			String response = ResponseBuilder.buildDone(s);
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("changes")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecordsModified(@QueryParam("since") String parTime) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);
			Date since = Utils.parseTimeUTC(parTime);
			List<Versioned<FoodItem>> items = foodbaseService.findChanged(userId, since);
			String s = serializer.writeAll(items);
			String response = ResponseBuilder.buildDone(s);
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response saveRecords(@FormParam("items") String parItems) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);
			List<Versioned<FoodItem>> items = serializer.readAll(parItems);
			foodbaseService.save(userId, items);

			String response = ResponseBuilder.buildDone("Saved OK");
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}
}
