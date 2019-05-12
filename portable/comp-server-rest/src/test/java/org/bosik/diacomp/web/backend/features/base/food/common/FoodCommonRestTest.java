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
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import java.util.Date;
import java.util.List;

import static org.bosik.diacomp.web.backend.features.base.food.FoodItemDataUtil.buildDemoData;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(FoodCommonRest.class)
public class FoodCommonRestTest
{

	private final Serializer<Versioned<FoodItem>> serializer = new SerializerFoodItem();

	@Autowired
	private MockMvc mvc;

	@MockBean
	private FoodCommonLocalService foodCommonService;

	@Test
	public void find_all() throws Exception
	{
		// given
		List<Versioned<FoodItem>> data = buildDemoData();
		when(foodCommonService.find()).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.URL));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<FoodItem>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void find_changed() throws Exception
	{
		// given
		Date time = new Date();
		List<Versioned<FoodItem>> data = buildDemoData();
		when(foodCommonService.findChanged(any())).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.URL).param(Api.PARAM_FIND_LAST_MODIFIED, Utils.formatTimeUTC(time)));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<FoodItem>> actual = serializer.readAll(response);
		assertEquals(data, actual);

	}

	@Test
	public void find_changed_tooLong() throws Exception
	{
		// given
		List<Versioned<FoodItem>> data = buildDemoData();
		when(foodCommonService.find()).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.URL).param(Api.PARAM_FIND_LAST_MODIFIED, Utils.buildString(20)));

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void find_changed_wayTooLong() throws Exception
	{
		// given
		List<Versioned<FoodItem>> data = buildDemoData();
		when(foodCommonService.find()).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.URL).param(Api.PARAM_FIND_LAST_MODIFIED, Utils.buildString(10 * 1024 * 1024)));

		// then
		request.andExpect(status().isBadRequest());
	}
}
