package org.bosik.diacomp.web.backend.features.auth.rest;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.UnsupportedAPIException;
import org.bosik.diacomp.web.backend.common.UserSessionUtils;
import org.bosik.diacomp.web.backend.features.auth.function.AuthDAO;
import org.bosik.diacomp.web.backend.features.auth.function.MySQLAuthDAO;

//FIXME: change all queryParam's to form-based in all POST requests

@Path("auth/")
public class AuthRestService
{
	@Context
	HttpServletRequest	req;

	private AuthDAO		authDao	= new MySQLAuthDAO();

	@POST
	@Path("login")
	@Produces(MediaType.APPLICATION_JSON)
	public Response login(@FormParam("login") String login, @FormParam("pass") String pass,
			@FormParam("api") @DefaultValue("-1") int apiVersion)
	{
		if (login == null)
		{
			// TODO: change to BAD_REQUEST
			String resp = ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Parameter 'login' is missing");
			return Response.ok(resp).build();
		}
		if (pass == null)
		{
			// TODO: change to BAD_REQUEST
			String resp = ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Parameter 'pass' is missing");
			return Response.ok(resp).build();
		}
		if (apiVersion == -1)
		{
			// TODO: change to BAD_REQUEST
			String resp = ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Parameter 'api' is missing");
			return Response.ok(resp).build();
		}

		try
		{
			int id = authDao.login(login, pass, apiVersion);
			UserSessionUtils.setId(req, id);

			String entity = ResponseBuilder.buildDone("Logged in OK");
			return Response.ok(entity).build();
		}
		catch (NotAuthorizedException e)
		{
			// Do not reset session flag here: anyone can reset your session otherwise

			// TODO: remove returning login:password back
			// THINK: should we use status 200 here? Isn't 401 better?
			String entity = ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS,
					String.format("Bad username/password (%s:%s)", login, pass));
			return Response.ok(entity).build();
		}
		catch (UnsupportedAPIException e)
		{
			String resp = ResponseBuilder.build(ResponseBuilder.CODE_UNSUPPORTED_API, e.getMessage());
			return Response.ok(resp).build();
		}
		catch (DeprecatedAPIException e)
		{
			String resp = ResponseBuilder.build(ResponseBuilder.CODE_DEPRECATED_API, e.getMessage());
			return Response.ok(resp).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	// TODO: GET version - just for debug purpose
	@GET
	@Path("login")
	@Produces(MediaType.APPLICATION_JSON)
	public Response loginDebug(@QueryParam("login") String login, @QueryParam("pass") String pass,
			@QueryParam("api") int apiVersion)
	{
		return login(login, pass, apiVersion);
	}

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
			e.printStackTrace();
			String entity = ResponseBuilder.buildFails();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(entity).build();
		}
	}
}
