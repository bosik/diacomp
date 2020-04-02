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
package org.bosik.diacomp.web.backend.features.user.auth;

import org.bosik.diacomp.core.services.exceptions.AuthException;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(AuthRest.class)
@RunWith(SpringRunner.class)
public class AuthRestTest
{
	private static final int    USER_ID     = 17;
	private static final String USERNAME    = "root@example.com";
	private static final String PASSWORD    = "qwerty";
	private static final String API_VERSION = "20";

	@Autowired
	private MockMvc mvc;

	@Test
	public void login_ok() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(post(Api.Auth.Login.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.param(Api.Auth.Login.PARAM_USERNAME, USERNAME)
				.param(Api.Auth.Login.PARAM_PASSWORD, PASSWORD)
				.param(Api.Auth.Login.PARAM_API_VERSION, API_VERSION));

		// then
		request.andExpect(status().isOk());
	}

	@Test
	public void login_wrong() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(post(Api.Auth.Login.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.param(Api.Auth.Login.PARAM_USERNAME, USERNAME)
				.param(Api.Auth.Login.PARAM_PASSWORD, "wrong")
				.param(Api.Auth.Login.PARAM_API_VERSION, API_VERSION));

		// then
		request.andExpect(status().isUnauthorized());
	}

	@TestConfiguration
	public static class TestConfig
	{
		@Bean
		public AuthService authService()
		{
			AuthService authService = mock(AuthService.class);
			when(authService.login(any(), any())).thenAnswer((e) ->
			{
				final String username = e.getArgument(0);
				final String password = e.getArgument(1);
				if (USERNAME.equalsIgnoreCase(username) && PASSWORD.equals(password))
				{
					return USER_ID;
				}
				else
				{
					throw new AuthException("Not authorized");
				}
			});
			return authService;
		}

		@Bean
		public AuthProvider authProvider(AuthService authService)
		{
			return new AuthProvider(authService);
		}

		@Bean
		public UserInfoService userInfoService()
		{
			return mock(UserInfoService.class);
		}
	}
}
