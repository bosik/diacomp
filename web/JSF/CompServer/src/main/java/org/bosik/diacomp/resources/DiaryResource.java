package org.bosik.diacomp.resources;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

@Path("diary")
public class DiaryResource
{
	@Context
	HttpServletRequest	req;

	@GET
	@Path("/login")
	@Produces(MediaType.APPLICATION_JSON)
	public String getIt()
	{
		HttpSession session = req.getSession(true);
		Object foo = session.getAttribute("foo");
		if (foo != null)
		{
			return foo.toString();
		}
		else
		{
			return "No session objects found";
		}
	}

	@GET
	@Path("/put")
	@Produces(MediaType.APPLICATION_JSON)
	public String put(@DefaultValue("") @QueryParam("value") String value)
	{
		HttpSession session = req.getSession(true);
		session.setAttribute("foo", value);

		return "Value " + value + " putted";
	}
}
