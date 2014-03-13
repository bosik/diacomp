package org.bosik.diacomp.web.backend.features.foodbase.rest;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.ready.SerializerFoodItem;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;

@Path("food/")
public class FoodBaseRestService
{
	@Context
	HttpServletRequest										req;

	//	private static final Parser<String>					parserString				= new Parser<String>()
	//																					{
	//																						// As-is
	//																						// "parser"
	//
	//																						@Override
	//																						public String read(
	//																								JSONObject json)
	//																								throws JSONException
	//																						{
	//																							if (json == null)
	//																							{
	//																								return null;
	//																							}
	//																							else
	//																							{
	//																								return json.toString();
	//																							}
	//																						}
	//
	//																						@Override
	//																						public JSONObject write(
	//																								String object)
	//																								throws JSONException
	//																						{
	//																							if (object == null)
	//																							{
	//																								return null;
	//																							}
	//																							else
	//																							{
	//																								return new JSONObject(
	//																										object);
	//																							}
	//																						}
	//																					};
	//	private static final Parser<Versioned<String>>		parserVersionedString		= new ParserVersioned<String>(
	//																							parserString);
	//	private static final Serializer<Versioned<String>>	serializerVersionedString	= new SerializerAdapter<Versioned<String>>(
	//																							parserVersionedString);

	private static final Serializer<Versioned<FoodItem>>	serializer	= new SerializerFoodItem();

	// @PUT
	// @Path("update")
	// @Produces(MediaType.APPLICATION_JSON)
	// public String put(@DefaultValue("") @QueryParam("value") String value)
	// {
	// HttpSession session = req.getSession(true);
	// session.setAttribute("foo", value);
	//
	// return "Value " + value + " putted";
	// }

	@GET
	@Path("test")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecords() throws CommonServiceException
	{
		try
		{
			//int userId = authService.getCurrentUserId(req);

			//Date time = Utils.parseTimeUTC(stime);
			//List<Versioned<String>> list = diaryService.findMod(userId, time);
			//String items = serializerVersionedString.writeAll(list);
			String response = ResponseBuilder.buildDone("It works");
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}
}
