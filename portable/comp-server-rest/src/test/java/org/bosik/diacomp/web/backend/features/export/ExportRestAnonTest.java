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

import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import org.bosik.diacomp.web.backend.features.base.dish.DishBaseLocalService;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodComboLocalService;
import org.bosik.diacomp.web.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.web.backend.features.preferences.PreferencesLocalService;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.rules.SpringClassRule;
import org.springframework.test.context.junit4.rules.SpringMethodRule;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(ExportRest.class)
@RunWith(JUnitParamsRunner.class)
public class ExportRestAnonTest
{
	@ClassRule
	public static final SpringClassRule SPRING_CLASS_RULE = new SpringClassRule();

	@Rule
	public final SpringMethodRule springMethodRule = new SpringMethodRule();

	@Autowired
	private MockMvc mvc;

	@MockBean
	private UserInfoService userInfoService;

	@MockBean
	private DiaryLocalService diaryLocalService;

	@MockBean
	private FoodComboLocalService foodComboLocalService;

	@MockBean
	private DishBaseLocalService dishBaseLocalService;

	@MockBean
	private PreferencesLocalService preferencesLocalService;

	@Test
	@Parameters(value = {//
			Api.Plain.URL, //
			Api.Json.URL //
	})
	public void shouldBeSecured(String url) throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(url));

		// then
		request.andExpect(status().isUnauthorized());
	}
}
