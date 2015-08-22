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
package org.bosik.diacomp.web.backend.features.diary;

import java.util.Date;
import java.util.List;
import java.util.Map;
import javax.ws.rs.Consumes;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.UserLogger;
import org.bosik.merklesync.DataSource;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Path("diary/")
public class DiaryRest
{
	@Autowired
	private DiaryService								diaryService;

	@Autowired
	private UserLogger									log;

	private final Parser<DiaryRecord>					parser				= new ParserDiaryRecord();
	private final Parser<Versioned<DiaryRecord>>		parserVersioned		= new ParserVersioned<DiaryRecord>(parser);
	private final Serializer<Versioned<DiaryRecord>>	serializer			= new SerializerAdapter<Versioned<DiaryRecord>>(
																					parserVersioned);
	private final Serializer<Map<String, String>>		serializerMap		= new SerializerMap();

	private static final int							MAX_DATETIME_SIZE	= Utils.FORMAT_DATE_TIME.length();

	/**
	 * Checks if the string value fits the maximum size. Null-safe.
	 * 
	 * @param s
	 * @param maxSize
	 * @return
	 */
	private String checkSize(String s, int maxSize)
	{
		if (s == null)
		{
			return null;
		}

		final int originalLength = s.length();
		if (originalLength > maxSize)
		{
			s = s.substring(0, maxSize);
			log.getLogger().warn(
					String.format("Parameter too long: %d chars passed, truncated down to %d: %s", originalLength,
							maxSize, s));
		}

		return s;
	}

	@GET
	@Path("count/{prefix: .*}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response count(@PathParam("prefix") @DefaultValue("") String parPrefix) throws CommonServiceException
	{
		long time = System.currentTimeMillis();
		try
		{
			parPrefix = checkSize(parPrefix, DataSource.ID_FULL_SIZE);

			int count = diaryService.count(parPrefix);
			String response = String.valueOf(count);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			log.getLogger().error(String.format("count(%s) failed", parPrefix), e);
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
		finally
		{
			time = System.currentTimeMillis() - time;
			System.out.println(String.format("count(%s) performed in %d msec", parPrefix, time));
		}
	}

	@GET
	@Path("guid/{guid: .*}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response findById(@PathParam("guid") String parId) throws CommonServiceException
	{
		long time = System.currentTimeMillis();
		try
		{
			parId = checkSize(parId, DataSource.ID_FULL_SIZE);

			// Prefix form
			if (parId.length() <= DataSource.ID_PREFIX_SIZE)
			{
				List<Versioned<DiaryRecord>> items = diaryService.findByIdPrefix(parId);

				String response = serializer.writeAll(items);
				return Response.ok(response).build();
			}

			// Full form
			else
			{
				Versioned<DiaryRecord> item = diaryService.findById(parId);

				if (item != null)
				{
					String response = serializer.write(item);
					return Response.ok(response).build();
				}
				else
				{
					String response = String.format("Item %s not found", parId);
					return Response.status(Status.NOT_FOUND).entity(response).build();
				}
			}
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (TooManyItemsException e)
		{
			return Response.status(Status.BAD_REQUEST).entity("Too many items found").build();
		}
		catch (Exception e)
		{
			log.getLogger().error(String.format("findById(%s) failed", parId), e);
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
		finally
		{
			time = System.currentTimeMillis() - time;
			System.out.println(String.format("findById(%s) performed in %d msec", parId, time));
		}
	}

	@GET
	@Path("hash/{prefix: .*}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getHash(@PathParam("prefix") @DefaultValue("") String parPrefix) throws CommonServiceException
	{
		try
		{
			parPrefix = checkSize(parPrefix, DataSource.ID_FULL_SIZE);

			String s = diaryService.getHash(parPrefix);
			String response = s != null ? s : "";
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			log.getLogger().error(String.format("hash(%s) failed", parPrefix), e);
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("hashes/{prefix: .*}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getHashChildren(@PathParam("prefix") @DefaultValue("") String parPrefix)
			throws CommonServiceException
	{
		try
		{
			parPrefix = checkSize(parPrefix, DataSource.ID_FULL_SIZE);

			Map<String, String> map = diaryService.getHashChildren(parPrefix);
			String response = serializerMap.write(map);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			log.getLogger().error(String.format("hashes(%s) failed", parPrefix), e);
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("changes")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response findChanged(@QueryParam("since") String parTime) throws CommonServiceException
	{
		try
		{
			if (parTime == null)
			{
				return Response.status(Status.BAD_REQUEST).entity("Missing parameter: since").build();
			}

			parTime = checkSize(parTime, MAX_DATETIME_SIZE);

			Date since;
			try
			{
				since = Utils.parseTimeUTC(parTime);
			}
			catch (Exception e) // FIXME: catch ParseException
			{
				String msg = String.format("Invalid time: %s%nExpected format: %s", parTime, Utils.FORMAT_DATE_TIME);
				return Response.status(Status.BAD_REQUEST).entity(msg).build();
			}

			List<Versioned<DiaryRecord>> items = diaryService.findChanged(since);
			String response = serializer.writeAll(items);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			log.getLogger().error(String.format("changes(%s) failed", parTime), e);
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("period")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response findPeriod(@QueryParam("start_time") String parStartTime,
			@QueryParam("end_time") String parEndTime, @QueryParam("show_rem") @DefaultValue("false") String parShowRem)
			throws CommonServiceException
	{
		try
		{
			if (parStartTime == null)
			{
				return Response.status(Status.BAD_REQUEST).entity("Missing parameter: start_time").build();
			}
			if (parEndTime == null)
			{
				return Response.status(Status.BAD_REQUEST).entity("Missing parameter: end_time").build();
			}

			parStartTime = checkSize(parStartTime, MAX_DATETIME_SIZE);
			parEndTime = checkSize(parEndTime, MAX_DATETIME_SIZE);

			Date startTime;
			try
			{
				startTime = Utils.parseTimeUTC(parStartTime);
			}
			catch (Exception e) // FIXME: catch ParseException
			{
				String msg = String.format("Invalid start time: %s%nExpected format: %s", parStartTime,
						Utils.FORMAT_DATE_TIME);
				return Response.status(Status.BAD_REQUEST).entity(msg).build();
			}

			Date endTime;
			try
			{
				endTime = Utils.parseTimeUTC(parEndTime);
			}
			catch (Exception e) // FIXME: catch ParseException
			{
				String msg = String.format("Invalid end time: %s%nExpected format: %s", parEndTime,
						Utils.FORMAT_DATE_TIME);
				return Response.status(Status.BAD_REQUEST).entity(msg).build();
			}

			boolean includeRemoved = Boolean.valueOf(parShowRem);

			List<Versioned<DiaryRecord>> items = diaryService.findPeriod(startTime, endTime, includeRemoved);
			String response = serializer.writeAll(items);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (TooManyItemsException e)
		{
			return Response.status(Status.BAD_REQUEST).entity("Too many items found").build();
		}
		catch (Exception e)
		{
			log.getLogger().error(String.format("period(%s, %s) failed", parStartTime, parEndTime), e);
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@PUT
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response save(@FormParam("items") String parItems) throws CommonServiceException
	{
		try
		{
			if (parItems == null)
			{
				return Response.status(Status.BAD_REQUEST).entity("Missing parameter: items").build();
			}

			// FIXME: limit the maximum data size

			List<Versioned<DiaryRecord>> items = serializer.readAll(parItems);
			diaryService.save(items);

			String response = "Saved OK";
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			log.getLogger().error(String.format("save() failed"), e);
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}
}
