/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.system;

import java.util.Date;
import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.user.auth.AuthRest;
import org.json.JSONObject;

@SuppressWarnings("static-method")
@Path("")
public class SystemRest
{
	@GET
	@Path("")
	@Produces(MediaType.TEXT_PLAIN)
	public Response welcome()
	{
		try
		{
			return Response.ok("Diacomp REST API is up").build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("system/info")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAPIVersion()
	{
		try
		{
			// Can't use StdResponse as far as response is JSON, not regular escaped string

			JSONObject info = new JSONObject();
			info.put("current", AuthRest.API_CURRENT);
			info.put("support", AuthRest.API_LEGACY);
			info.put("build", Config.get(Config.KEY_BUILD_TIME));

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

	@GET
	@Path("system/time")
	@Produces(MediaType.TEXT_PLAIN)
	public Response getTime()
	{
		try
		{
			String s = Utils.formatTimeUTC(new Date());
			return Response.ok(s).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			String entity = ResponseBuilder.buildFails();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(entity).build();
		}
	}
}
