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
import org.bosik.diacomp.core.utils.ZipUtils;
import org.bosik.diacomp.core.utils.ZipUtils.Entry;
import org.bosik.diacomp.web.backend.features.base.dish.DishBaseLocalService;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodComboLocalService;
import org.bosik.diacomp.web.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.web.backend.features.preferences.PreferencesLocalService;
import org.bosik.diacomp.web.backend.features.user.auth.UserRest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.ws.rs.core.HttpHeaders;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

@RestController
@RequestMapping("/export")
public class ExportRest extends UserRest
{
	private static final String FILE_NAME          = "data.zip";
	private static final String HEADER_DISPOSITION = "attachment; filename=\"" + FILE_NAME + "\"";

	@Autowired
	private DiaryLocalService diaryService;

	@Autowired
	private FoodComboLocalService foodComboLocalService;

	@Autowired
	private DishBaseLocalService dishbaseService;

	@Autowired
	private PreferencesLocalService prefService;

	private static byte[] getBytes(String s) throws UnsupportedEncodingException
	{
		return s.getBytes("UTF-8");
	}

	@GetMapping(path = "/json", produces = "application/zip")
	public ResponseEntity<InputStreamResource> exportJson() throws IOException
	{
		final int userId = getUserId();

		List<Entry> entries = new ArrayList<>();
		entries.add(new Entry(ExportAPI.JSON_DIARY, getBytes(diaryService.exportJson(userId))));
		entries.add(new Entry(ExportAPI.JSON_FOODBASE, getBytes(foodComboLocalService.exportJson(userId))));
		entries.add(new Entry(ExportAPI.JSON_DISHBASE, getBytes(dishbaseService.exportJson(userId))));
		entries.add(new Entry(ExportAPI.JSON_PREFERENCES, getBytes(prefService.exportJson(userId))));

		final InputStreamResource data = new InputStreamResource(ZipUtils.zip(entries));
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, HEADER_DISPOSITION)
				.body(data);
	}

	@GetMapping(path = "/plain", produces = "application/zip")
	public ResponseEntity<InputStreamResource> exportPlain() throws IOException
	{
		final int userId = getUserId();

		final List<Entry> entries = new ArrayList<>();
		entries.add(new Entry(ExportAPI.PLAIN_DIARY, getBytes(diaryService.exportPlain(userId))));
		entries.add(new Entry(ExportAPI.PLAIN_FOODBASE, getBytes(foodComboLocalService.exportPlain(userId))));
		entries.add(new Entry(ExportAPI.PLAIN_DISHBASE, getBytes(dishbaseService.exportPlain(userId))));
		entries.add(new Entry(ExportAPI.PLAIN_PREFERENCES, getBytes(prefService.exportPlain(userId))));

		final InputStreamResource data = new InputStreamResource(ZipUtils.zip(entries));
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, HEADER_DISPOSITION)
				.body(data);
	}
}
