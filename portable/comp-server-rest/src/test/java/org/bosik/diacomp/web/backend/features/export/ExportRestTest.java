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

import org.bosik.diacomp.web.backend.features.base.dish.DishBaseLocalService;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodComboLocalService;
import org.bosik.diacomp.web.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.web.backend.features.preferences.PreferencesLocalService;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(ExportRest.class)
@WithMockUser(roles = "USER")
public class ExportRestTest
{
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
	public void exportAsJson() throws Exception
	{
		// given
		when(diaryLocalService.exportJson(anyInt())).thenReturn("(diary data)");
		when(foodComboLocalService.exportJson(anyInt())).thenReturn("(food data)");
		when(dishBaseLocalService.exportJson(anyInt())).thenReturn("(dish data)");
		when(preferencesLocalService.exportJson(anyInt())).thenReturn("(preferences data)");

		// when
		ResultActions request = mvc.perform(get(Api.Json.URL));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		assertNotNull(response); // TODO: make deeper check
	}

	@Test
	public void exportAsPlain() throws Exception
	{
		// given
		when(diaryLocalService.exportPlain(anyInt())).thenReturn("(diary data)");
		when(foodComboLocalService.exportPlain(anyInt())).thenReturn("(food data)");
		when(dishBaseLocalService.exportPlain(anyInt())).thenReturn("(dish data)");
		when(preferencesLocalService.exportPlain(anyInt())).thenReturn("(preferences data)");

		// when
		ResultActions request = mvc.perform(get(Api.Plain.URL));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		assertNotNull(response); // TODO: make deeper check
	}
}
