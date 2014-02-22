package org.bosik.diacomp.features.diary;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.bo.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.common.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Parser;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.ready.SerializerDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.serializers.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.features.auth.UserService;
import org.bosik.diacomp.features.diary.dao.DiaryDAO;
import org.bosik.diacomp.features.diary.dao.MySQLDiaryDAO;
import org.bosik.diacomp.utils.ResponseBuilder;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

@Path("diary/")
public class DiaryResource
{
	@Context
	HttpServletRequest										req;

	private DiaryDAO										diaryService				= new MySQLDiaryDAO();

	private static final Parser<String>						parserString				= new Parser<String>()
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
																									return json
																											.toString();
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
	private static final Parser<Versioned<String>>			parserVersionedString		= new ParserVersioned<String>(
																								parserString);
	private static final Serializer<Versioned<String>>		serializerVersionedString	= new SerializerAdapter<Versioned<String>>(
																								parserVersionedString);

	private static final Serializer<Versioned<DiaryRecord>>	serializerVersionedRecord	= new SerializerDiaryRecord();

	@GET
	@Path("guid")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecords(@QueryParam("guids") String sGUID) throws CommonServiceException
	{
		try
		{
			int userId = UserService.getId(req);

			List<Versioned<String>> respList = new ArrayList<Versioned<String>>();
			JSONArray json = new JSONArray(sGUID);
			for (int i = 0; i < json.length(); i++)
			{
				String guid = json.get(i).toString();
				List<Versioned<String>> temp = diaryService.findGuid(userId, guid);
				if (!temp.isEmpty())
				{
					respList.add(temp.get(0));
				}
			}

			String items = serializerVersionedString.writeAll(respList);
			String response = ResponseBuilder.buildDone(items);
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			// FIXME: remove error info from response bean
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails(e.getMessage()))
					.build();
		}
	}

	@GET
	@Path("new")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecords(@QueryParam("mod_after") String stime, @QueryParam("show_rem") String parShowRem)
			throws CommonServiceException
	{
		try
		{
			int userId = UserService.getId(req);
			Date time = Utils.parseTimeUTC(stime);
			boolean includeRemoved = Boolean.valueOf(parShowRem); // TODO: use common formatter
			List<Versioned<String>> list = diaryService.findMod(userId, time, includeRemoved);
			String items = serializerVersionedString.writeAll(list);
			String response = ResponseBuilder.buildDone(items);
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("period")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecords(@QueryParam("start_time") String parStartTime,
			@QueryParam("end_time") String parEndTime, @QueryParam("show_rem") String parShowRem)
			throws CommonServiceException
	{
		try
		{
			int userId = UserService.getId(req);
			Date startTime = Utils.parseTimeUTC(parStartTime);
			Date endTime = Utils.parseTimeUTC(parEndTime);
			boolean includeRemoved = Boolean.valueOf(parShowRem);

			List<Versioned<String>> list = diaryService.findPeriod(userId, startTime, endTime, includeRemoved);
			String items = serializerVersionedString.writeAll(list);
			String response = ResponseBuilder.buildDone(items);
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	public Response post(String entity) throws CommonServiceException
	{
		try
		{
			int userId = UserService.getId(req);
			List<Versioned<DiaryRecord>> itemList = serializerVersionedRecord.readAll(entity);
			diaryService.post(userId, itemList);

			String response = ResponseBuilder.buildDone("Saved OK");
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			// FIXME: remove error info from response bean
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails(e.getMessage()))
					.build();
		}
	}
}
