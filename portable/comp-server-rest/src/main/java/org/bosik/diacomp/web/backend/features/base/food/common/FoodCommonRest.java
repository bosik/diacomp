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
package org.bosik.diacomp.web.backend.features.base.food.common;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import java.util.Date;
import java.util.List;

@RestController
@RequestMapping("/food/common")
public class FoodCommonRest
{
	private static final String TYPE_JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=utf-8";

	@Autowired
	private FoodCommonLocalService foodCommonService;

	@GetMapping
	@Produces(TYPE_JSON_UTF8)
	public List<Versioned<FoodItem>> find(@RequestParam(name = "lastModified", required = false) String time)
	{
		if (time != null)
		{
			Utils.checkSize(time, Utils.FORMAT_DATE_TIME.length());
			Date lastModified = Utils.parseTimeUTC(time);
			return foodCommonService.findChanged(lastModified);
		}
		else
		{
			return foodCommonService.find();
		}
	}
}
