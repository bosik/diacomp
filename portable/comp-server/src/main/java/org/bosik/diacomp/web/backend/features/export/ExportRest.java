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

import org.bosik.diacomp.core.rest.ExportAPI;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.utils.ZipUtils;
import org.bosik.diacomp.core.utils.ZipUtils.Entry;
import org.bosik.diacomp.web.backend.features.base.dish.DishBaseLocalService;
import org.bosik.diacomp.web.backend.features.base.food.FoodBaseLocalService;
import org.bosik.diacomp.web.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.web.backend.features.preferences.PreferencesLocalService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import java.util.ArrayList;
import java.util.List;

@Service
@Path("export")
public class ExportRest
{
	@Autowired
	private DiaryLocalService diaryService;

	@Autowired
	private FoodBaseLocalService foodbaseService;

	@Autowired
	private DishBaseLocalService dishbaseService;

	@Autowired
	private PreferencesLocalService prefService;

	@GET
	@Path("/json")
	@Produces("application/zip")
	public Response exportJson()
	{
		try
		{
			List<Entry> entries = new ArrayList<>();
			entries.add(new Entry(ExportAPI.JSON_DIARY, diaryService.exportData().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.JSON_FOODBASE, foodbaseService.exportData().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.JSON_DISHBASE, dishbaseService.exportData().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.JSON_PREFERENCES, prefService.exportData().getBytes("UTF-8")));

			return Response.ok(ZipUtils.zip(entries)).header("Content-Disposition", "attachment; filename=\"data.zip\"").build();
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
	@Path("/plain")
	@Produces("application/zip")
	public Response exportPlain()
	{
		try
		{
			List<Entry> entries = new ArrayList<>();
			entries.add(new Entry(ExportAPI.PLAIN_DIARY, diaryService.exportPlain().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.PLAIN_FOODBASE, foodbaseService.exportPlain().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.PLAIN_DISHBASE, dishbaseService.exportPlain().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.PLAIN_PREFERENCES, prefService.exportPlain().getBytes("UTF-8")));

			return Response.ok(ZipUtils.zip(entries)).header("Content-Disposition", "attachment; filename=\"data.zip\"").build();
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
}
