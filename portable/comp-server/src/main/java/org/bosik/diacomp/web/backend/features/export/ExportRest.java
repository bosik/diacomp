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
package org.bosik.diacomp.web.backend.features.export;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.core.utils.ZipUtils;
import org.bosik.diacomp.core.utils.ZipUtils.Entry;
import org.bosik.diacomp.web.backend.features.base.dish.DishBaseLocalService;
import org.bosik.diacomp.web.backend.features.base.food.FoodBaseLocalService;
import org.bosik.diacomp.web.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.web.backend.features.preferences.PreferencesLocalService;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Path("export/")
public class ExportRest
{
	// PART OF PUBLIC API
	public static final String			ENTRY_DIARY			= "diary.json";
	public static final String			ENTRY_FOODBASE		= "foodbase.json";
	public static final String			ENTRY_DISHBASE		= "dishbase.json";
	public static final String			ENTRY_PREFERENCES	= "preferences.json";

	@Autowired
	private DiaryLocalService			diaryService;

	@Autowired
	private FoodBaseLocalService		foodbaseService;

	@Autowired
	private DishBaseLocalService		dishbaseService;

	@Autowired
	private PreferencesLocalService		preferencesService;

	private final Parser<DiaryRecord>	parser				= new ParserDiaryRecord();

	@GET
	@Produces("application/zip")
	public Response export()
	{
		try
		{
			List<Entry> entries = new ArrayList<Entry>();
			entries.add(new Entry(ENTRY_DIARY, diaryService.exportData()));
			entries.add(new Entry(ENTRY_FOODBASE, foodbaseService.exportData()));
			entries.add(new Entry(ENTRY_DISHBASE, dishbaseService.exportData()));
			entries.add(new Entry(ENTRY_PREFERENCES, preferencesService.exportData()));

			return Response.ok(ZipUtils.zip(entries)).header("Content-Disposition", "attachment; filename=\"data.zip\"")
					.build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("windows")
	@Produces(MediaType.TEXT_PLAIN + ";charset=utf-8")
	public Response exportForDesktop()
	{
		try
		{
			List<Entry> entries = new ArrayList<Entry>();
			entries.add(new Entry("Diary.txt", exportDiaryForWindows()));

			return Response.ok(ZipUtils.zip(entries)).header("Content-Disposition", "attachment; filename=\"data.zip\"")
					.build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	private String exportDiaryForWindows()
	{
		Serializer<DiaryRecord> serializer = new SerializerAdapter<DiaryRecord>(parser);

		List<Versioned<DiaryRecord>> items = diaryService.findPeriod(new Date(0, 0, 1), new Date(200, 0, 1), true);

		StringBuilder s = new StringBuilder();

		s.append("VERSION=5\n");
		for (Versioned<DiaryRecord> item : items)
		{
			s.append(Utils.formatTimeUTC(item.getData().getTime())).append('\t');
			s.append(Utils.formatTimeUTC(item.getTimeStamp())).append('\t');
			s.append(item.getHash()).append('\t');
			s.append(item.getId()).append('\t');
			s.append(item.getVersion()).append('\t');
			s.append(item.isDeleted() ? "true" : "false").append('\t');
			s.append(serializer.write(item.getData())).append('\n');
		}

		String data = s.toString();
		return data;
	}
}
