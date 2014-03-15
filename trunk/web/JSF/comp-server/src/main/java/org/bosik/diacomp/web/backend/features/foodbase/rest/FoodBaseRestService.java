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
import org.bosik.diacomp.core.persistence.serializers.ready.SerializerFoodItem;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.UserSessionUtils;
import org.bosik.diacomp.web.backend.features.foodbase.function.FoodbaseDAO;
import org.bosik.diacomp.web.backend.features.foodbase.function.MySQLFoodbaseDAO;

@Path("food/")
public class FoodBaseRestService
{
	@Context
	HttpServletRequest										req;

	private final FoodbaseDAO								foodbaseService	= new MySQLFoodbaseDAO();

	private static final Serializer<Versioned<FoodItem>>	serializer	= new SerializerFoodItem();

	@GET
	@Path("guid/{guid}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecordsGuid(@PathParam("guid") String parGuid) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);

			Versioned<FoodItem> item = foodbaseService.findByGuid(userId, parGuid);
			String s = (item != null) ? serializer.write(item) : "";
			// TODO: use "not found", not just empty string
			String response = ResponseBuilder.buildDone(s);
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			// FIXME: remove error info from response bean
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails(e.getMessage()))
					.build();
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
	public Response postRecords(@FormParam("items") String parItems) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);
			List<Versioned<FoodItem>> items = serializer.readAll(parItems);
			foodbaseService.post(userId, items);

			String response = ResponseBuilder.buildDone("Saved OK");
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			// FIXME: remove error info from response bean
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails(e.getMessage()))
					.build();
		}
	}
}
