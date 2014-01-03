package org.bosik.diacomp.resources;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import org.bosik.diacomp.utils.ResponseBuilder;

@Path("auth")
public class AuthResource
{
	private static final String	PAR_USERID		= "USER_ID";
	private static final int	INVALID_USER	= -1;

	@Context
	HttpServletRequest			req;

	// TODO: seems bad approach
	public static boolean checkAuth(HttpServletRequest request)
	{
		return (request != null) && (request.getSession(false) != null)
				&& (request.getSession().getAttribute(PAR_USERID) != null)
		/* && (!request.getSession().getAttribute(PAR_USERID).equals(INVALID_USER)) */;
	}

	public static int getCurrentUserId(HttpServletRequest request)
	{
		return (Integer) request.getSession().getAttribute(PAR_USERID);
	}

	private static int authentificate(String login, String pass)
	{
		if ("admin".equals(login) && "1234".equals(pass))
		{
			return 42;
		}

		return INVALID_USER;
	}

	@POST
	@Path("/login_post")
	@Produces(MediaType.APPLICATION_JSON)
	public Response login(@QueryParam("login") String login, @QueryParam("pass") String pass)
	{
		int id = authentificate(login, pass);

		if (id != INVALID_USER)
		{
			req.getSession().setAttribute(PAR_USERID, id);
			String entity = ResponseBuilder.buildDone("Logged in OK");
			return Response.ok(entity).build();
		}
		else
		{
			// TODO: remove returning login:password back
			String entity = ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS,
					String.format("Bad username/password (%s:%s)", login, pass));
			return Response.ok(entity).build();
		}
	}

	@GET
	@Path("/logout")
	@Produces(MediaType.APPLICATION_JSON)
	public Response logout()
	{
		req.getSession().setAttribute(PAR_USERID, null);
		String entity = ResponseBuilder.buildDone("Logged out OK");
		return Response.ok(entity).build();
	}
}
