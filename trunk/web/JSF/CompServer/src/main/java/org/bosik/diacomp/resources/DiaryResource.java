package org.bosik.diacomp.resources;

import java.util.List;
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
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.serializers.Parser;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.persistence.serializers.utils.ParserVersioned;
import org.bosik.compensation.persistence.serializers.utils.SerializerAdapter;
import org.bosik.diacomp.services.DiaryService;
import org.bosik.diacomp.utils.ResponseBuilder;
import org.json.JSONException;
import org.json.JSONObject;

@Path("diary")
public class DiaryResource
{
	@Context
	HttpServletRequest									req;

	private DiaryService								diaryService				= new DiaryService();

	private static final Parser<String>					parserString				= new Parser<String>()
																					{
																						// As-is
																						// "parser"

																						@Override
																						public String read(
																								JSONObject json)
																								throws JSONException
																						{
																							if (json == null)
																							{
																								return null;
																							}
																							else
																							{
																								return json.toString();
																							}
																						}

																						@Override
																						public JSONObject write(
																								String object)
																								throws JSONException
																						{
																							if (object == null)
																							{
																								return null;
																							}
																							else
																							{
																								return new JSONObject(
																										object);
																							}
																						}
																					};
	private static final Parser<Versioned<String>>		parserVersionedString		= new ParserVersioned<String>(
																							parserString);
	private static final Serializer<Versioned<String>>	serializerVersionedString	= new SerializerAdapter<Versioned<String>>(
																							parserVersionedString);

	@GET
	@Path("/view")
	@Produces(MediaType.APPLICATION_JSON)
	public String demoView()
	{
		try
		{
			int userId = AuthResource.getCurrentUserId(req);
			List<Versioned<String>> list = diaryService.findAll(userId);
			return serializerVersionedString.writeAll(list);
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return ResponseBuilder.buildFails();
		}
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
