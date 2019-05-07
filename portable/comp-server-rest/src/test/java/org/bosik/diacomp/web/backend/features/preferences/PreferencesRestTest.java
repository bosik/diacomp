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
package org.bosik.diacomp.web.backend.features.preferences;

import org.bosik.diacomp.core.persistence.parsers.ParserPreferenceEntry;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(PreferencesRest.class)
@WithMockUser(roles = "USER")
public class PreferencesRestTest
{
	public static class Api
	{
		public static final String BASE_URL = "/preferences";

		public static class GetAll
		{
			public static final String URL = BASE_URL;
		}

		public static class GetString
		{
			public static final String URL = BASE_URL;
		}

		public static class Hash
		{
			public static final String URL = BASE_URL + "/hash";
		}

		public static class Save
		{
			public static final String URL         = BASE_URL;
			public static final String PARAM_DATA  = "data";
			public static final String RESPONSE_OK = "Saved OK";
		}
	}

	private final Serializer<PreferenceEntry<String>> serializer = new SerializerAdapter<>(new ParserPreferenceEntry());

	@Autowired
	private MockMvc mvc;

	@MockBean
	private PreferencesLocalService preferencesLocalService;

	@MockBean
	private UserInfoService userInfoService;

	@Test
	public void getAll() throws Exception
	{
		// given
		List<PreferenceEntry<String>> data = buildDemoData();
		when(preferencesLocalService.getAll(anyInt())).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.GetAll.URL));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<PreferenceEntry<String>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void getString() throws Exception
	{
		// given
		PreferenceEntry<String> data = buildDemoData().get(0);
		when(preferencesLocalService.getString(anyInt(), eq(PreferenceID.RATES_MASS_UNITS))).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.GetString.URL + "/" + PreferenceID.RATES_MASS_UNITS.getCode()));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		PreferenceEntry<String> actual = serializer.read(response);
		assertEquals(data, actual);
	}

	@Test
	public void getString_badRequest() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.GetString.URL + "/123456"));

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void getHash() throws Exception
	{
		// given
		String hash = "42";
		when(preferencesLocalService.getHash(anyInt())).thenReturn(hash);

		// when
		ResultActions request = mvc.perform(get(Api.Hash.URL));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		assertEquals(hash, response);
	}

	@Test
	public void save() throws Exception
	{
		// given
		final int userId = 17;
		when(userInfoService.getCurrentUserId()).thenReturn(userId);
		List<PreferenceEntry<String>> data = buildDemoData();

		// when
		MockHttpServletRequestBuilder r = put(Api.Save.URL);
		r.contentType(MediaType.APPLICATION_FORM_URLENCODED);
		r.param(Api.Save.PARAM_DATA, serializer.writeAll(data));
		ResultActions request = mvc.perform(r);

		// then
		request.andExpect(status().isOk()).andExpect(content().string(Api.Save.RESPONSE_OK));
		verify(preferencesLocalService).update(eq(userId), any());
	}

	private static List<PreferenceEntry<String>> buildDemoData() throws ParseException
	{
		return new ArrayList<PreferenceEntry<String>>()
		{{
			add(new PreferenceEntry<String>()
			{{
				setId(PreferenceID.RATES_MASS_UNITS);
				setVersion(4);
				setValue("56ce855fac9846698a78edf2cacf4cfe");
			}});
			add(new PreferenceEntry<String>()
			{{
				setId(PreferenceID.ANDROID_MEAL_FORMAT);
				setVersion(20);
				setValue("f2dbaeeafffb487496d953c619e9479e");
			}});
		}};
	}
}
