package org.bosik.diacomp.backend.resources;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.utils.ResponseBuilder;
import org.json.JSONObject;

@Path("/")
public class SystemResource
{
	@GET
	@Path("info")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAPIVersion()
	{
		try
		{
			JSONObject r = new JSONObject();
			r.put("code", "0");
			r.put("current", "2");
			r.put("support", "2");

			String resp = r.toString();
			return Response.ok(resp).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			String entity = ResponseBuilder.buildFails();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(entity).build();
		}
	}
}
