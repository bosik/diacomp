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
package org.bosik.diacomp.web.backend.features.diary;

import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.merklesync.Versioned;
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

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(DiaryRest.class)
@RunWith(JUnitParamsRunner.class)
public class DiaryRestAnonTest
{
	@ClassRule
	public static final SpringClassRule SPRING_CLASS_RULE = new SpringClassRule();

	@Rule
	public final SpringMethodRule springMethodRule = new SpringMethodRule();

	private final Serializer<Versioned<DiaryRecord>> serializer = new SerializerAdapter<>(new ParserVersioned<>(new ParserDiaryRecord()));

	@Autowired
	private MockMvc mvc;

	@MockBean
	private UserInfoService userInfoService;

	@MockBean
	private DiaryLocalService diaryLocalService;

	@Test
	@Parameters(value = {//
			Api.Count.URL, //
			Api.Count.URL + "/ea", //
			Api.Count.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa", //
			Api.FindById.URL, //
			Api.FindById.URL + "/ff", //
			Api.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931fa", //
			Api.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa", //
			Api.FindPeriod.URL, //
			Api.FindPeriod.URL + "?" + Api.FindPeriod.PARAM_FROM + "=2019-04-01 21:00:00&" + Api.FindPeriod.PARAM_TO
					+ "=2019-04-02 21:00:00", //
			Api.FindPeriod.URL + "?" + Api.FindPeriod.PARAM_FROM + "=2019-04-01 21:00:000", //
			Api.FindChanged.URL, //
			Api.FindChanged.URL + "?" + Api.FindChanged.PARAM_SINCE + "=2019-04-16 20:10:32", //
			Api.FindChanged.URL + "?" + Api.FindChanged.PARAM_SINCE + "=2019-04-16 20:10:320", //
			Api.Hash.URL, //
			Api.Hash.URL + "/f2", //
			Api.Hash.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa", //
			Api.Hashes.URL + "/f", //
			Api.Hashes.URL + "/abcd" //
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
		List<Versioned<DiaryRecord>> data = DiaryDataUtil.buildDemoData();

		// when
		MockHttpServletRequestBuilder r = put(Api.Save.URL);
		r.contentType(MediaType.APPLICATION_FORM_URLENCODED);
		r.param(Api.Save.PARAM_DATA, serializer.writeAll(data));
		ResultActions request = mvc.perform(r);

		// then
		request.andExpect(status().isUnauthorized());
	}

	@Test
	public void shouldBeSecured_save_missing() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(put(Api.Save.URL).contentType(MediaType.APPLICATION_FORM_URLENCODED));

		// then
		request.andExpect(status().isUnauthorized());
	}
}
