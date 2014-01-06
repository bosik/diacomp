package org.bosik.diacomp.resources;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import org.bosik.diacomp.MySQLAccess;

@Path("diary")
public class DiaryResource
{
	@Context
	HttpServletRequest	req;

	@GET
	@Path("/view")
	@Produces(MediaType.APPLICATION_JSON)
	public String demoView()
	{
		int id = AuthResource.getCurrentUserId(req);

		try
		{
			MySQLAccess dao = new MySQLAccess();
			dao.select(MySQLAccess.TABLE_DIARY, "");
		}
		catch (Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return "Diary of the user #" + id;
	}

	@PUT
	@Path("/update")
	@Produces(MediaType.APPLICATION_JSON)
	public String put(@DefaultValue("") @QueryParam("value") String value)
	{
		HttpSession session = req.getSession(true);
		session.setAttribute("foo", value);

		return "Value " + value + " putted";
	}
}
