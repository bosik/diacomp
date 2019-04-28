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
package org.bosik.diacomp.web.backend.features.base.dish;

import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDishItem;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
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

import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.net.URLEncoder.encode;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(DishBaseRest.class)
@WithMockUser(roles = "USER")
public class DishBaseRestTest
{
	public static class Api
	{
		public static final String BASE_URL = "/dish";

		public static class Count
		{
			public static final String URL = BASE_URL + "/count";
		}

		public static class FindById
		{
			public static final String URL = BASE_URL + "/guid";
		}

		public static class FindAny
		{
			public static final String URL          = BASE_URL + "/search";
			public static final String PARAM_FILTER = "q";
		}

		public static class FindAll
		{
			public static final String URL                   = BASE_URL + "/all";
			public static final String PARAM_INCLUDE_REMOVED = "show_rem";
		}

		public static class FindChanged
		{
			public static final String URL         = BASE_URL + "/changes";
			public static final String PARAM_SINCE = "since";
		}

		public static class Hash
		{
			public static final String URL = BASE_URL + "/hash";
		}

		public static class Hashes
		{
			public static final String URL = BASE_URL + "/hashes";
		}

		public static class Save
		{
			public static final String URL         = BASE_URL;
			public static final String PARAM_DATA  = "items";
			public static final String RESPONSE_OK = "Saved OK";
		}
	}

	private final Serializer<Map<String, String>> serializerMap = new SerializerMap();
	private final Serializer<Versioned<DishItem>> serializer    = new SerializerDishItem();

	@Autowired
	private MockMvc mvc;

	@MockBean
	private DishBaseLocalService dishBaseLocalService;

	@MockBean
	private UserInfoService userInfoService;

	@Test
	public void count_findAll() throws Exception
	{
		// given
		when(dishBaseLocalService.count(anyInt())).thenReturn(42);

		// when
		ResultActions request = mvc.perform(get(Api.Count.URL));

		// then
		request.andExpect(status().isOk()).andExpect(content().string("42"));
	}

	@Test
	public void count_findByPrefix() throws Exception
	{
		// given
		when(dishBaseLocalService.count(anyInt(), eq("ea"))).thenReturn(12);

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
		List<Versioned<DishItem>> data = buildDemoData();
		when(dishBaseLocalService.findAll(anyInt(), eq(true))).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.FindById.URL));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DishItem>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void findById_prefix() throws Exception
	{
		// given
		List<Versioned<DishItem>> data = buildDemoData();
		when(dishBaseLocalService.findByIdPrefix(anyInt(), eq("ff"))).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.FindById.URL + "/ff"));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DishItem>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void findById_single() throws Exception
	{
		// given
		Versioned<DishItem> data = buildDemoData().get(0);
		when(dishBaseLocalService.findById(anyInt(), eq("542a8a10ef1a41ecb9338dbeb4a931fa"))).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931fa"));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		Versioned<DishItem> actual = serializer.read(response);
		assertEquals(data, actual);
	}

	@Test
	public void findById_single_notFound() throws Exception
	{
		// given
		when(dishBaseLocalService.findById(anyInt(), eq("542a8a10ef1a41ecb9338dbeb4a931fa"))).thenReturn(null);

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
	public void findAll_nonDeleted() throws Exception
	{
		// given
		List<Versioned<DishItem>> data = buildDemoData();
		when(dishBaseLocalService.findAll(anyInt(), eq(false))).thenReturn(data);

		// when
		ResultActions request = mvc.perform(get(Api.FindAll.URL));

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DishItem>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void findAll_includeDeleted() throws Exception
	{
		// given
		List<Versioned<DishItem>> data = buildDemoData();
		when(dishBaseLocalService.findAll(anyInt(), eq(true))).thenReturn(data);

		// when
		MockHttpServletRequestBuilder r = get(Api.FindAll.URL);
		r.param(Api.FindAll.PARAM_INCLUDE_REMOVED, "true");
		ResultActions request = mvc.perform(r);

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DishItem>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void findAll_badRequest_tooLong() throws Exception
	{
		// given / when
		MockHttpServletRequestBuilder r = get(Api.FindAll.URL);
		r.param(Api.FindAll.PARAM_INCLUDE_REMOVED, Utils.buildString(6));
		ResultActions request = mvc.perform(r);

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void findAny() throws Exception
	{
		// given
		String query = "ban";
		List<Versioned<DishItem>> data = buildDemoData().stream()
														.filter(d -> d.getData().getName().toLowerCase().contains(query.toLowerCase()))
														.collect(Collectors.toList());
		assertTrue("Data must contain at least one element", !data.isEmpty());
		when(dishBaseLocalService.findAny(anyInt(), eq(query))).thenReturn(data);

		// when
		MockHttpServletRequestBuilder r = get(Api.FindAny.URL);
		r.param(Api.FindAny.PARAM_FILTER, query);
		ResultActions request = mvc.perform(r);

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DishItem>> actual = serializer.readAll(response);
		assertEquals(data, actual);
	}

	@Test
	public void findAny_badRequest_missing() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(get(Api.FindAny.URL));

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void findAny_badRequest_tooLong() throws Exception
	{
		// given / when
		MockHttpServletRequestBuilder r = get(Api.FindAny.URL);
		r.param(Api.FindAny.PARAM_FILTER, Utils.buildString(257));
		ResultActions request = mvc.perform(r);

		// then
		request.andExpect(status().isBadRequest());
	}

	@Test
	public void findChanged() throws Exception
	{
		// given
		Date since = new Date();
		List<Versioned<DishItem>> data = buildDemoData();
		when(dishBaseLocalService.findChanged(anyInt(), any())).thenReturn(data);

		// when
		MockHttpServletRequestBuilder r = get(Api.FindChanged.URL);
		r.param(Api.FindChanged.PARAM_SINCE, Utils.formatDateUTC(since));
		ResultActions request = mvc.perform(r);

		// then
		String response = request.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
		List<Versioned<DishItem>> actual = serializer.readAll(response);
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
		when(dishBaseLocalService.getHashTree(eq(userId))).thenReturn(hashTree);

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
		when(dishBaseLocalService.getHashTree(eq(userId))).thenReturn(hashTree);

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
		when(dishBaseLocalService.getHashTree(eq(userId))).thenReturn(hashTree);

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
		when(dishBaseLocalService.getHashTree(eq(userId))).thenReturn(new MemoryMerkleTree3(new HashMap<>()));

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
		List<Versioned<DishItem>> data = buildDemoData();

		// when
		MockHttpServletRequestBuilder r = put(Api.Save.URL);
		r.contentType(MediaType.APPLICATION_FORM_URLENCODED);
		r.with(csrf());
		r.param(Api.Save.PARAM_DATA, serializer.writeAll(data));
		ResultActions request = mvc.perform(r);

		// then
		request.andExpect(status().isOk()).andExpect(content().string(Api.Save.RESPONSE_OK));
		verify(dishBaseLocalService).save(eq(userId), any());
	}

	private String buildUrlEncodedFormEntity(Map<String, String> data)
	{
		final StringBuilder result = new StringBuilder();
		final String UTF8 = StandardCharsets.UTF_8.name();

		for (Map.Entry<String, String> entry : data.entrySet())
		{
			if (result.length() > 0)
			{
				result.append('&');
			}

			try
			{
				result.append(encode(entry.getKey(), UTF8)).append('=').append(encode(entry.getValue(), UTF8));
			}
			catch (UnsupportedEncodingException e)
			{
				throw new RuntimeException(e);
			}
		}
		return result.toString();
	}

	@Test
	public void save_badRequest_missing() throws Exception
	{
		// given / when
		ResultActions request = mvc.perform(put(Api.Save.URL).contentType(MediaType.APPLICATION_FORM_URLENCODED).with(csrf()));

		// then
		request.andExpect(status().isBadRequest());
		verify(dishBaseLocalService, times(0)).save(anyInt(), any());
	}

	private static List<Versioned<DishItem>> buildDemoData()
	{
		return new ArrayList<Versioned<DishItem>>()
		{{
			add(new Versioned<DishItem>()
			{{
				setId("1");
				setTimeStamp(new Date());
				setHash("hash");
				setVersion(13);
				setDeleted(false);
				setData(new DishItem()
				{{
					setName("Apple pie");
					setMass(165);
					getContent().add(new FoodMassed()
					{{
						setName("Apple");
						setRelProts(0.2);
						setRelFats(0.1);
						setRelCarbs(11.2);
						setRelValue(40);
						setMass(2000);
					}});
				}});
			}});
			add(new Versioned<DishItem>()
			{{
				setId("2");
				setTimeStamp(new Date());
				setHash("hash");
				setVersion(13);
				setDeleted(false);
				setData(new DishItem()
				{{
					setName("Banana");
					setMass(120);
				}});
			}});
		}};
	}
}
