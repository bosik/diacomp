package org.bosik.diacomp.web.backend.features.system.rest;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.web.backend.utils.ResponseBuilder;
import org.json.JSONObject;

@Path("info/")
@SuppressWarnings("static-method")
public class SystemResource
{
	@GET
	// @Path("")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAPIVersion()
	{
		try
		{
			JSONObject info = new JSONObject();
			info.put("current", "2");
			info.put("support", "2");

			JSONObject r = new JSONObject();
			r.put("code", "0");
			r.put("resp", info);

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
