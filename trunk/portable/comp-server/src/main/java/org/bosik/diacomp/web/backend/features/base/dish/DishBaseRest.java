package org.bosik.diacomp.web.backend.features.base.dish;

import java.util.Date;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DefaultValue;
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
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDishItem;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.UserSessionUtils;

@Path("dish/")
public class DishBaseRest
{
	@Context
	HttpServletRequest								req;

	private final DishBaseService					dishbaseService	= new DishBaseLocalService()
																	{
																		@Override
																		protected int getCurrentUserId()
																		{
																			return UserSessionUtils.getId(req);
																		};
																	};

	private final Serializer<Versioned<DishItem>>	serializer		= new SerializerDishItem();
	private final Serializer<Map<String, String>>	serializerMap	= new SerializerMap();

	@GET
	@Path("guid/{guid}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response findById(@PathParam("guid") String parId) throws CommonServiceException
	{
		try
		{
			// Prefix form
			if (parId.length() == ObjectService.ID_PREFIX_SIZE)
			{
				List<Versioned<DishItem>> items = dishbaseService.findByIdPrefix(parId);

				String s = serializer.writeAll(items);
				String response = ResponseBuilder.buildDone(s);
				return Response.ok(response).build();
			}

			// Full form
			else
			{
				Versioned<DishItem> item = dishbaseService.findById(parId);

				if (item != null)
				{
					String s = serializer.write(item);
					String response = ResponseBuilder.buildDone(s);
					return Response.ok(response).build();
				}
				else
				{
					String response = ResponseBuilder.build(ResponseBuilder.CODE_NOTFOUND,
							String.format("Item %s not found", parId));
					return Response.ok(response).build();
				}
			}
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.OK).entity(ResponseBuilder.buildNotAuthorized()).build();
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
	public Response findAll(@QueryParam("show_rem") String parShowRem) throws CommonServiceException
	{
		try
		{
			boolean includeRemoved = Boolean.valueOf(parShowRem);

			List<Versioned<DishItem>> items = dishbaseService.findAll(includeRemoved);
			String s = serializer.writeAll(items);
			String response = ResponseBuilder.buildDone(s);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.OK).entity(ResponseBuilder.buildNotAuthorized()).build();
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
	public Response findAny(@QueryParam("q") String filter) throws CommonServiceException
	{
		try
		{
			List<Versioned<DishItem>> items = dishbaseService.findAny(filter);
			String s = serializer.writeAll(items);
			String response = ResponseBuilder.buildDone(s);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.OK).entity(ResponseBuilder.buildNotAuthorized()).build();
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
	public Response findChanged(@QueryParam("since") String parTime) throws CommonServiceException
	{
		try
		{
			Date since = Utils.parseTimeUTC(parTime);
			List<Versioned<DishItem>> items = dishbaseService.findChanged(since);
			String s = serializer.writeAll(items);
			String response = ResponseBuilder.buildDone(s);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.OK).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("hash/{prefix: .*}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getHash(@PathParam("prefix") @DefaultValue("") String parPrefix) throws CommonServiceException
	{
		try
		{
			String s = dishbaseService.getHash(parPrefix);
			String response = ResponseBuilder.buildDone(s != null ? s : "");
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.OK).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("hashes/{prefix: .*}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getHashChildren(@PathParam("prefix") @DefaultValue("") String parPrefix)
			throws CommonServiceException
	{
		try
		{
			Map<String, String> map = dishbaseService.getHashChildren(parPrefix);
			String s = serializerMap.write(map);
			String response = ResponseBuilder.buildDone(s);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.OK).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response save(@FormParam("items") String parItems) throws CommonServiceException
	{
		try
		{
			List<Versioned<DishItem>> items = serializer.readAll(parItems);
			dishbaseService.save(items);

			String response = ResponseBuilder.buildDone("Saved OK");
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.OK).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}
}