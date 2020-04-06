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
package org.bosik.diacomp.web.backend.features.system;

import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.log.LogService;
import org.hamcrest.core.StringContains;
import org.hamcrest.core.StringStartsWith;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(SystemRest.class)
public class SystemRestTest
{
	public static class Api
	{
		public static final String BASE_URL = "";

		public static class Welcome
		{
			public static final String URL = BASE_URL + "/";
		}

		public static class Info
		{
			public static final String URL = BASE_URL + "/system/info";
		}

		public static class Time
		{
			public static final String URL = BASE_URL + "/system/time";
		}
	}

	@Autowired
	private MockMvc mvc;

	@MockBean
	private LogService logService;

	@Test
	public void welcome() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.Welcome.URL));

		// then
		request.andExpect(status().isOk()).andExpect(content().string(new StringStartsWith("Diacomp REST API is up")));
	}

	@Test
	public void info() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.Info.URL));

		// then
		request.andExpect(status().isOk()).andExpect(content().string(new StringContains("current")));
	}

	@Test
	public void time() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.Time.URL));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		Utils.parseTimeUTC(response);
	}

	@Test
	public void missing() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get("/bad"));

		// then
		request.andExpect(status().isNotFound());
		//.andExpect(content().string("Not Found"));
	}
}
