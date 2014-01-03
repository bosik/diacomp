package org.bosik.diacomp.resources;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
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

	// FIXME: change to POST
	@GET
	@Path("/login")
	@Produces(MediaType.APPLICATION_JSON)
	public String getIt(@QueryParam("login") String login, @QueryParam("pass") String pass)
	{
		int id = authentificate(login, pass);

		if (id != INVALID_USER)
		{
			req.getSession().setAttribute(PAR_USERID, id);
			return ResponseBuilder.buildDone("Logged in OK");
		}
		else
		{
			return ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS,
					String.format("Bad username/password (%s:%s)", login, pass));
		}
	}

	private static int authentificate(String login, String pass)
	{
		if ("admin".equals(login) && "1234".equals(pass))
		{
			return 42;
		}

		return INVALID_USER;
	}
}
