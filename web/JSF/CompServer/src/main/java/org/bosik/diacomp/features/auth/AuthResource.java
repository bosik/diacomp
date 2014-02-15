package org.bosik.diacomp.features.auth;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.features.auth.dao.AuthDAO;
import org.bosik.diacomp.features.auth.dao.FakeAuthDAO;
import org.bosik.diacomp.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.services.exceptions.UnsupportedAPIException;
import org.bosik.diacomp.utils.ResponseBuilder;

@Path("auth/")
public class AuthResource
{
	@Context
	HttpServletRequest	req;

	private AuthDAO		authService	= new FakeAuthDAO();

	@POST
	@Path("login")
	@Produces(MediaType.APPLICATION_JSON)
	public Response login(@QueryParam("login") String login, @QueryParam("pass") String pass,
			@QueryParam("api") int apiVersion)
	{
		try
		{
			authService.login(req, login, pass, apiVersion);
			String entity = ResponseBuilder.buildDone("Logged in OK");
			return Response.ok(entity).build();
		}
		catch (NotAuthorizedException e)
		{
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
			authService.logout(req);
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
