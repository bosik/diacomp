package org.bosik.diacomp.web.backend.features.auth.rest;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.common.UserSessionUtils;
import org.bosik.diacomp.web.backend.features.auth.function.AuthDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Path("auth/")
@Component
public class AuthRestService
{
	public static final int		API_CURRENT	= 20;
	public static final int		API_LEGACY	= 19;

	@Context
	private HttpServletRequest	req;

	@Autowired
	private AuthDAO				authDao;

	@POST
	@Path("login")
	@Produces(MediaType.APPLICATION_JSON)
	public Response login(@FormParam("login") String login, @FormParam("pass") String pass,
			@FormParam("api") @DefaultValue("-1") int apiVersion)
	{
		try
		{
			// check if all params are presented

			if (login == null)
			{
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Parameter 'login' is missing");
				return Response.status(Status.BAD_REQUEST).entity(resp).build();
			}
			if (pass == null)
			{
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Parameter 'pass' is missing");
				return Response.status(Status.BAD_REQUEST).entity(resp).build();
			}
			if (apiVersion == -1)
			{
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Parameter 'api' is missing");
				return Response.status(Status.BAD_REQUEST).entity(resp).build();
			}

			// check the values

			if (apiVersion < API_LEGACY)
			{
				String msg = String.format("API %d is unsupported. The latest API: %d. Legacy API: %d.", apiVersion,
						API_CURRENT, API_LEGACY);
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_UNSUPPORTED_API, msg);
				return Response.ok(resp).build();
			}

			if (apiVersion < API_CURRENT)
			{
				String msg = String.format(
						"API %d is still supported, but deprecated. The latest API: %d. Legacy API: %d.", apiVersion,
						API_CURRENT, API_LEGACY);
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_DEPRECATED_API, msg);
				return Response.ok(resp).build();
			}

			int id = authDao.login(login, pass);
			UserSessionUtils.setId(req, id, login);

			String entity = ResponseBuilder.buildDone("Logged in OK");
			return Response.ok(entity).build();
		}
		catch (NotAuthorizedException e)
		{
			// Do not reset session flag here: anyone can reset your session otherwise

			// THINK: should we use status 200 OK here? Isn't 401 better?
			String entity = ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS, "Bad username/password");
			return Response.ok(entity).build();
		}
		catch (Exception e)
		{
			// FIXME: FOR DEBUG PURPOSE ONLY
			e.printStackTrace();

			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	// @GET
	// @Path("login")
	// @Produces(MediaType.APPLICATION_JSON)
	// public Response loginDebug(@QueryParam("login") String login, @QueryParam("pass") String
	// pass,
	// @QueryParam("api") int apiVersion)
	// {
	// return login(login, pass, apiVersion);
	// }

	@GET
	@Path("logout")
	@Produces(MediaType.APPLICATION_JSON)
	public Response logout()
	{
		try
		{
			UserSessionUtils.clearId(req);
			String entity = ResponseBuilder.buildDone("Logged out OK");
			return Response.ok(entity).build();
		}
		catch (Exception e)
		{
			// FIXME: FOR DEBUG PURPOSE ONLY
			e.printStackTrace();

			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}
}
