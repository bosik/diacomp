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
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.rest.ExportAPI;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
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
@Path("export")
public class ExportRest
{
	@Autowired
	private DiaryLocalService		diaryService;

	@Autowired
	private FoodBaseLocalService	foodbaseService;

	@Autowired
	private DishBaseLocalService	dishbaseService;

	@Autowired
	private PreferencesLocalService	prefService;

	@GET
	@Path("/android")
	@Produces("application/zip")
	public Response exportAndroid()
	{
		try
		{
			List<Entry> entries = new ArrayList<Entry>();
			entries.add(new Entry(ExportAPI.ANDROID_JSON_DIARY, diaryService.exportData().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.ANDROID_JSON_FOODBASE, foodbaseService.exportData().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.ANDROID_JSON_DISHBASE, dishbaseService.exportData().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.ANDROID_JSON_PREFERENCES, prefService.exportData().getBytes("UTF-8")));

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
	@Path("/android/plain")
	@Produces("application/zip")
	public Response exportAndroidPlain()
	{
		try
		{
			List<Entry> entries = new ArrayList<Entry>();
			entries.add(new Entry(ExportAPI.ANDROID_PLAIN_DIARY, exportPlainDiary().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.ANDROID_PLAIN_FOODBASE, exportPlainFoodbase().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.ANDROID_PLAIN_DISHBASE, exportPlainDishbase().getBytes("UTF-8")));
			entries.add(new Entry(ExportAPI.ANDROID_PLAIN_PREFERENCES, exportPlainPreferences().getBytes("UTF-8")));

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
	@Path("/windows")
	@Produces("application/zip")
	public Response exportWindows()
	{
		try
		{
			List<Entry> entries = new ArrayList<Entry>();
			entries.add(new Entry("Diary.txt", exportPlainDiary().getBytes("UTF-8")));

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

	private String exportPlainDiary()
	{
		Serializer<DiaryRecord> serializer = new SerializerAdapter<DiaryRecord>(new ParserDiaryRecord());

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

		return s.toString();
	}

	private String exportPlainFoodbase()
	{
		Serializer<FoodItem> serializer = new SerializerAdapter<FoodItem>(new ParserFoodItem());

		StringBuilder s = new StringBuilder();

		s.append("VERSION=1\n");
		for (Versioned<FoodItem> item : foodbaseService.findAll(true))
		{
			s.append(removeTabs(item.getData().getName())).append('\t');
			s.append(item.getId()).append('\t');
			s.append(Utils.formatTimeUTC(item.getTimeStamp())).append('\t');
			s.append(item.getHash()).append('\t');
			s.append(item.getVersion()).append('\t');
			s.append(item.isDeleted() ? "true" : "false").append('\t');
			s.append(serializer.write(item.getData())).append('\n');
		}

		return s.toString();
	}

	private String exportPlainDishbase()
	{
		Serializer<DishItem> serializer = new SerializerAdapter<DishItem>(new ParserDishItem());

		StringBuilder s = new StringBuilder();

		s.append("VERSION=1\n");
		for (Versioned<DishItem> item : dishbaseService.findAll(true))
		{
			s.append(removeTabs(item.getData().getName())).append('\t');
			s.append(item.getId()).append('\t');
			s.append(Utils.formatTimeUTC(item.getTimeStamp())).append('\t');
			s.append(item.getHash()).append('\t');
			s.append(item.getVersion()).append('\t');
			s.append(item.isDeleted() ? "true" : "false").append('\t');
			s.append(serializer.write(item.getData())).append('\n');
		}

		return s.toString();
	}

	private String exportPlainPreferences()
	{
		StringBuilder s = new StringBuilder();

		s.append("VERSION=1\n");
		for (PreferenceEntry<String> item : prefService.getAll())
		{
			s.append(item.getType().getKey()).append('\t');
			s.append(item.getVersion()).append('\t');
			s.append(item.getValue()).append('\n');
		}

		return s.toString();
	}

	private static String removeTabs(String name)
	{
		return name.replaceAll("\t", "");
	}
}
