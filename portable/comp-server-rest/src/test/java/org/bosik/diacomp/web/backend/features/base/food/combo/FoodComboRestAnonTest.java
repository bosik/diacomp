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
package org.bosik.diacomp.web.backend.features.base.food.combo;

import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.base.food.FoodItemDataUtil;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.merklesync.Versioned;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.rules.SpringClassRule;
import org.springframework.test.context.junit4.rules.SpringMethodRule;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.util.List;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(FoodComboRest.class)
@RunWith(JUnitParamsRunner.class)
public class FoodComboRestAnonTest
{
	@ClassRule
	public static final SpringClassRule SPRING_CLASS_RULE = new SpringClassRule();

	@Rule
	public final SpringMethodRule springMethodRule = new SpringMethodRule();

	private final Serializer<Versioned<FoodItem>> serializer = new SerializerFoodItem();

	@Autowired
	private MockMvc mvc;

	@MockBean
	private FoodComboLocalService comboLocalService;

	@MockBean
	private UserInfoService userInfoService;

	@Before
	public void init()
	{
		when(userInfoService.getCurrentUserId()).thenThrow(new NotAuthorizedException());
	}

	@Test
	@Parameters(value = {//
			Api.Food.Count.URL, //
			Api.Food.Count.URL + "/ea", //
			Api.Food.Count.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa", //
			Api.Food.FindById.URL, //
			Api.Food.FindById.URL + "/ff", //
			Api.Food.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931fa", //
			Api.Food.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa", //
			Api.Food.FindAny.URL + "?" + Api.Food.FindAny.PARAM_FILTER + "=tes", //
			Api.Food.FindAll.URL, //
			Api.Food.FindAll.URL + "?" + Api.Food.FindAll.PARAM_INCLUDE_REMOVED + "true", //
			Api.Food.FindChanged.URL + "?" + Api.Food.FindChanged.PARAM_SINCE + "=2019-04-16 20:10:32", //
			Api.Food.FindChanged.URL + "?" + Api.Food.FindChanged.PARAM_SINCE + "=2019-04-16 20:10:320", //
			Api.Food.Hash.URL, //
			Api.Food.Hash.URL + "/f2", //
			Api.Food.Hash.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa", //
			Api.Food.Hashes.URL + "/f", //
			Api.Food.Hashes.URL + "/abcd" //
	})
	public void shouldBeSecured(String url) throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(url));

		// then
		request.andExpect(status().isUnauthorized());
	}

	@Test
	public void shouldBeSecured_save() throws Exception
	{
		// given
		List<Versioned<FoodItem>> data = FoodItemDataUtil.buildDemoData();

		// when
		MockHttpServletRequestBuilder r = put(Api.Food.Save.URL);
		r.contentType(MediaType.APPLICATION_FORM_URLENCODED);
		r.param(Api.Food.Save.PARAM_DATA, serializer.writeAll(data));
		ResultActions request = mvc.perform(r);

		// then
		request.andExpect(status().isUnauthorized());
	}
}
