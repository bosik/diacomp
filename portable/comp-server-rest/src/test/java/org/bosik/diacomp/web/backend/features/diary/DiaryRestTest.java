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

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.merklesync.MemoryMerkleTree3;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
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

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(DiaryRest.class)
@WithMockUser(roles = "USER")
public class DiaryRestTest
{
	private final Serializer<Map<String, String>>    serializerMap = new SerializerMap();
	private final Serializer<Versioned<DiaryRecord>> serializer    = new SerializerAdapter<>(
			new ParserVersioned<>(new ParserDiaryRecord()));

	@Autowired
	private MockMvc mvc;

	@MockBean
	private DiaryLocalService diaryLocalService;

	@MockBean
	private UserInfoService userInfoService;

	@Test
	public void count_findAll() throws Exception
	{
		// given
		when(diaryLocalService.count(anyInt())).thenReturn(42);

		// when
		ResultActions request = mvc.perform(get(Api.Count.URL));

		// then
		request.andExpect(status().isOk()).andExpect(content().string("42"));
	}

	@Test
	public void count_findByPrefix() throws Exception
	{
		// given
		when(diaryLocalService.count(anyInt(), eq("ea"))).thenReturn(12);

		// when
		ResultActions request = mvc.perform(get(Api.Count.URL + "/ea"));

		// then
		request.andExpect(status().isOk()).andExpect(content().string("12"));
	}

	@Test
	public void count_badRequest() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.Count.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa"));

		// then
		request.andExpect(status().isBadRequest())
			   .andExpect(content().string("String too long: 33 chars passed, but at most 32 are allowed"));
	}

	@Test
	public void findById_all() throws Exception
	{
		// given
		List<Versioned<DiaryRecord>> data = DiaryDataUtil.buildDemoData();
		when(diaryLocalService.findAll(anyInt(), eq(true))).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.FindById.URL));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DiaryRecord>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void findById_prefix() throws Exception
	{
		// given
		List<Versioned<DiaryRecord>> data = DiaryDataUtil.buildDemoData();
		when(diaryLocalService.findByIdPrefix(anyInt(), eq("ff"))).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.FindById.URL + "/ff"));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DiaryRecord>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void findById_single() throws Exception
	{
		// given
		Versioned<DiaryRecord> data = DiaryDataUtil.buildDemoData().get(0);
		when(diaryLocalService.findById(anyInt(), eq("542a8a10ef1a41ecb9338dbeb4a931fa"))).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931fa"));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		Versioned<DiaryRecord> actual = serializer.read(response);
		assertEquals(data, actual);
	}

	@Test
	public void findById_single_notFound() throws Exception
	{
		// given
		when(diaryLocalService.findById(anyInt(), eq("542a8a10ef1a41ecb9338dbeb4a931fa"))).thenReturn(null);

		// when
		ResultActions request = mvc.perform(get(Api.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931fa"));

		// then
		request.andExpect(status().isNotFound()).andExpect(content().string("Item '542a8a10ef1a41ecb9338dbeb4a931fa' not found"));
	}

	@Test
	public void findById_badRequest() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa"));

		// then
		request.andExpect(status().isBadRequest())
			   .andExpect(content().string("String too long: 33 chars passed, but at most 32 are allowed"));
	}

	@Test
	public void findPeriod() throws Exception
	{
		// given
		String from = "2019-04-01 21:00:00";
		String to = "2019-04-02 21:00:00";
		List<Versioned<DiaryRecord>> data = DiaryDataUtil.buildDemoData();
		when(diaryLocalService.findPeriod(anyInt(), any(), any(), eq(false))).thenReturn(data);

		// when
		MockHttpServletRequestBuilder r = get(Api.FindPeriod.URL);
		r.param(Api.FindPeriod.PARAM_FROM, from);
		r.param(Api.FindPeriod.PARAM_TO, to);
		ResultActions request = mvc.perform(r);

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DiaryRecord>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void findAny_badRequest_missing() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.FindPeriod.URL));

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void findAny_badRequest_tooLong() throws Exception
	{
		// given / when
		MockHttpServletRequestBuilder r = get(Api.FindPeriod.URL);
		r.param(Api.FindPeriod.PARAM_FROM, Utils.buildString(20));
		ResultActions request = mvc.perform(r);

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void findChanged() throws Exception
	{
		// given
		Date since = new Date();
		List<Versioned<DiaryRecord>> data = DiaryDataUtil.buildDemoData();
		when(diaryLocalService.findChanged(anyInt(), any())).thenReturn(data);

		// when
		MockHttpServletRequestBuilder r = get(Api.FindChanged.URL);
		r.param(Api.FindChanged.PARAM_SINCE, Utils.formatDateUTC(since));
		ResultActions request = mvc.perform(r);

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DiaryRecord>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void findChanged_badRequest_missing() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.FindChanged.URL));

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void findChanged_badRequest_tooLong() throws Exception
	{
		// given / when
		MockHttpServletRequestBuilder r = get(Api.FindChanged.URL);
		r.param(Api.FindChanged.PARAM_SINCE, Utils.buildString(20));
		ResultActions request = mvc.perform(r);

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void getHash_root() throws Exception
	{
		// given
		final int userId = 19;
		final String hash = "ec082942e2bd40f4a9e8cb0093531a89";

		MerkleTree hashTree = new MemoryMerkleTree3(new HashMap<String, String>()
		{{
			put("", hash);
		}});

		when(userInfoService.getCurrentUserId()).thenReturn(userId);
		when(diaryLocalService.getHashTree(eq(userId))).thenReturn(hashTree);

		// when
		ResultActions request = mvc.perform(get(Api.Hash.URL));

		// then
		request.andExpect(status().isOk()).andExpect(content().string(hash));
	}

	@Test
	public void getHash_prefix() throws Exception
	{
		// given
		final int userId = 17;
		final String prefix = "f2";
		final String hash = "ec082942e2bd40f4a9e8cb0093531a89";

		MerkleTree hashTree = new MemoryMerkleTree3(new HashMap<String, String>()
		{{
			put(prefix, hash);
		}});

		when(userInfoService.getCurrentUserId()).thenReturn(userId);
		when(diaryLocalService.getHashTree(eq(userId))).thenReturn(hashTree);

		// when
		ResultActions request = mvc.perform(get(Api.Hash.URL + "/" + prefix));

		// then
		request.andExpect(status().isOk()).andExpect(content().string(hash));
	}

	@Test
	public void getHash_badRequest_tooLong() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.Hash.URL + "/" + Utils.buildString(33)));

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void getHashChildren() throws Exception
	{
		// given
		final int userId = 17;
		final String prefix = "f";

		Map<String, String> data = new HashMap<String, String>()
		{{
			put(prefix + "0", "ec082942e2bd40f4a9e8cb0093531a89");
			put(prefix + "1", "2e1b142e395e4631bb5101ebc64a1367");
			put(prefix + "2", "77a5079709454aefa01da4a3674386b9");
			put(prefix + "3", "9395885d8c3742ff9890299c0367c789");
		}};
		MerkleTree hashTree = new MemoryMerkleTree3(data);

		when(userInfoService.getCurrentUserId()).thenReturn(userId);
		when(diaryLocalService.getHashTree(eq(userId))).thenReturn(hashTree);

		// when
		ResultActions request = mvc.perform(get(Api.Hashes.URL + "/" + prefix));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		Map<String, String> actual = serializerMap.read(response);
		assertEquals(data, actual);
	}

	@Test
	public void getHashChildren_badRequest_tooLong() throws Exception
	{
		// given
		final int userId = 17;
		when(userInfoService.getCurrentUserId()).thenReturn(userId);
		when(diaryLocalService.getHashTree(eq(userId))).thenReturn(new MemoryMerkleTree3(new HashMap<>()));

		// when
		ResultActions request = mvc.perform(get(Api.Hashes.URL + "/" + Utils.buildString(4)));

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void save() throws Exception
	{
		// given
		final int userId = 17;
		when(userInfoService.getCurrentUserId()).thenReturn(userId);
		List<Versioned<DiaryRecord>> data = DiaryDataUtil.buildDemoData();

		// when
		MockHttpServletRequestBuilder r = put(Api.Save.URL);
		r.contentType(MediaType.APPLICATION_FORM_URLENCODED);
		r.param(Api.Save.PARAM_DATA, serializer.writeAll(data));
		ResultActions request = mvc.perform(r);

		// then
		request.andExpect(status().isOk()).andExpect(content().string(Api.Save.RESPONSE_OK));
		verify(diaryLocalService).save(eq(userId), any());
	}

	@Test
	public void save_badRequest_missing() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(put(Api.Save.URL).contentType(MediaType.APPLICATION_FORM_URLENCODED));

		// then
		request.andExpect(status().isBadRequest());
		verify(diaryLocalService, times(0)).save(anyInt(), any());
	}
}