package org.bosik.diacomp.web.backend.features.diary.rest;

import java.util.Date;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDiaryRecord;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.UserSessionUtils;
import org.bosik.diacomp.web.backend.features.diary.function.DiaryDAO;
import org.bosik.diacomp.web.backend.features.diary.function.MySQLDiaryDAO;
import org.json.JSONException;
import org.json.JSONObject;

@Path("diary/")
public class DiaryRestService
{
	@Context
	HttpServletRequest										req;

	private final DiaryDAO									diaryService				= new MySQLDiaryDAO();

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
	@Path("guid/{guid}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecordsGuid(@PathParam("guid") String parGuid) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);

			Versioned<String> item = diaryService.findByGuid(userId, parGuid);
			String s = (item != null) ? serializerVersionedString.write(item) : "";
			// TODO: use "not found", not just empty string
			String response = ResponseBuilder.buildDone(s);
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
	@Path("changes")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getRecordsModified(@QueryParam("since") String parTime) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);
			Date since = Utils.parseTimeUTC(parTime);
			List<Versioned<String>> items = diaryService.findChanged(userId, since);
			String s = serializerVersionedString.writeAll(items);
			String response = ResponseBuilder.buildDone(s);
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
	public Response getRecordsPeriod(@QueryParam("start_time") String parStartTime,
			@QueryParam("end_time") String parEndTime, @QueryParam("show_rem") String parShowRem)
			throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);
			Date startTime = Utils.parseTimeUTC(parStartTime);
			Date endTime = Utils.parseTimeUTC(parEndTime);
			boolean includeRemoved = Boolean.valueOf(parShowRem);

			List<Versioned<String>> items = diaryService.findPeriod(userId, startTime, endTime, includeRemoved);
			String s = serializerVersionedString.writeAll(items);
			String response = ResponseBuilder.buildDone(s);
			return Response.ok(response).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response postRecords(@FormParam("items") String parItems) throws CommonServiceException
	{
		try
		{
			int userId = UserSessionUtils.getId(req);
			List<Versioned<DiaryRecord>> items = serializerVersionedRecord.readAll(parItems);
			diaryService.post(userId, items);

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
