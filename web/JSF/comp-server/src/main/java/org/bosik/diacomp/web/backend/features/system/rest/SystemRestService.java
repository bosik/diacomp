package org.bosik.diacomp.web.backend.features.system.rest;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.web.backend.features.auth.rest.AuthRestService;
import org.json.JSONObject;

@Path("info/")
@SuppressWarnings("static-method")
public class SystemRestService
{
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAPIVersion()
	{
		try
		{
			// Can't use StdResponse as far as response is JSON, not regular escaped string

			JSONObject info = new JSONObject();
			info.put("current", AuthRestService.API_CURRENT);
			info.put("support", AuthRestService.API_LEGACY);

			JSONObject resp = new JSONObject();
			resp.put(StdResponse.TAG_CODE, ResponseBuilder.CODE_OK);
			resp.put(StdResponse.TAG_RESPONSE, info);

			return Response.ok(resp.toString()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			String entity = ResponseBuilder.buildFails();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(entity).build();
		}
	}
}
