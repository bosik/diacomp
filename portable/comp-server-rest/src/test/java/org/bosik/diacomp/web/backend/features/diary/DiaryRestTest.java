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
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.IntegrationTest;
import org.bosik.merklesync.Versioned;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.reactive.server.WebTestClient.ResponseSpec;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.BodyInserters;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class DiaryRestTest extends IntegrationTest
{
	private final Serializer<Map<String, String>>    serializerMap = new SerializerMap();
	private final Serializer<Versioned<DiaryRecord>> serializer    = new SerializerAdapter<>(
			new ParserVersioned<>(new ParserDiaryRecord()));

	@Autowired
	private DiaryEntityRepository diaryEntityRepository;

	@Before
	public void onBefore()
	{
		super.onBefore();

		diaryEntityRepository.deleteAll();
		diaryEntityRepository.saveAll(buildData(getUserId()));
	}

	private static List<DiaryEntity> buildData(int userId)
	{
		return Arrays.asList(
				DiaryEntity.builder()
						.id("f906b4341cc54a59acf23ed0108164f0")
						.userId(userId)
						.timeStamp(new Date())
						.hash("7c47e50757a14528a2b0b6896cb03432")
						.version(1)
						.deleted(false)
						.content("{\"finger\":7,\"time\":\"2020-04-01 08:41:34\",\"type\":\"blood\",\"value\":5.6}")
						.timeCache(Utils.parseTimeUTC("2020-04-01 08:41:34"))
						.build(),
				DiaryEntity.builder()
						.id("5e2d537c5110494d90d7f5277f4367ca")
						.userId(userId)
						.timeStamp(new Date())
						.hash("821818e3ef4f41e3bcb3893364b38835")
						.version(1)
						.deleted(false)
						.content("{\"time\":\"2020-04-01 09:00:36\",\"type\":\"ins\",\"value\":12}")
						.timeCache(Utils.parseTimeUTC("2020-04-01 09:00:36"))
						.build(),
				DiaryEntity.builder()
						.id("f9455692ef294608a85c1d7d52fb6956")
						.userId(userId)
						.timeStamp(new Date())
						.hash("d6704d5506ab4104a5a5cab720af8736")
						.version(2)
						.deleted(true)
						.content("{\"time\":\"2020-03-30 01:15:40\",\"type\":\"ins\",\"value\":7}")
						.timeCache(Utils.parseTimeUTC("2020-03-30 01:15:40"))
						.build(),
				DiaryEntity.builder()
						.id("5e20de21655447ce849e42a1c51db615")
						.userId(userId + 1) // another user
						.timeStamp(new Date())
						.hash("684ad8d6363b4c1bbdb9e74b9472adc6")
						.version(1)
						.deleted(false)
						.content("{\"time\":\"2020-03-26 21:47:00\",\"text\":\"test\",\"type\":\"note\"}")
						.timeCache(Utils.parseTimeUTC("2020-03-26 21:47:00"))
						.build()
		);
	}

	@Test
	public void count_findAll() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.Count.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isOk()
				.expectBody(String.class)
				.isEqualTo("3");
	}

	@Test
	public void count_findByPrefix() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.Count.URL + "/5e")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isOk()
				.expectBody(String.class)
				.isEqualTo("1");
	}

	@Test
	public void count_badRequest() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get()
				.uri(URL_ROOT + Api.Diary.Count.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isBadRequest()
				.expectBody(String.class)
				.isEqualTo("String too long: 33 chars passed, but at most 32 are allowed");
	}

	@Test
	public void findById_all() throws Exception
	{
		// given
		final int userId = getUserId();
		final List<Versioned<DiaryRecord>> expected = DiaryLocalService.convert(
				diaryEntityRepository.findByUserId(userId));

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.FindById.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final List<Versioned<DiaryRecord>> actual = serializer.readAll(response);
		assertEqualsSorted(expected, actual);
	}

	@Test
	public void findById_prefix() throws Exception
	{
		// given
		final int userId = getUserId();
		final List<Versioned<DiaryRecord>> expected = DiaryLocalService.convert(
				diaryEntityRepository.findByUserIdAndIdStartingWith(userId, "5e"));

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.FindById.URL + "/5e")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final List<Versioned<DiaryRecord>> actual = serializer.readAll(response);
		assertEqualsSorted(expected, actual);
	}

	@Test
	public void findById_single() throws Exception
	{
		// given
		final int userId = getUserId();
		final Versioned<DiaryRecord> expected = DiaryLocalService.convert(
				diaryEntityRepository.findByUserIdAndId(userId, "f906b4341cc54a59acf23ed0108164f0"));

		// when
		final ResponseSpec result = webClient
				.get()
				.uri(URL_ROOT + Api.Diary.FindById.URL + "/f906b4341cc54a59acf23ed0108164f0")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final Versioned<DiaryRecord> actual = serializer.read(response);
		assertEquals(expected, actual);
	}

	@Test
	public void findById_single_notFound() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get()
				.uri(URL_ROOT + Api.Diary.FindById.URL + "/0d869e560e474f0bb5fcc46824313a62")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isNotFound()
				.expectBody(String.class)
				.isEqualTo("Item '0d869e560e474f0bb5fcc46824313a62' not found");
	}

	@Test
	public void findById_badRequest() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get()
				.uri(URL_ROOT + Api.Diary.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isBadRequest()
				.expectBody(String.class)
				.isEqualTo("String too long: 33 chars passed, but at most 32 are allowed");
	}

	@Test
	public void findPeriod() throws Exception
	{
		// given
		final int userId = getUserId();
		final String from = "2020-03-30 21:00:00";
		final String to = "2020-04-02 21:00:00";

		final List<Versioned<DiaryRecord>> expected = DiaryLocalService.convert(
				diaryEntityRepository.findForPeriod(
						userId,
						Utils.parseDateUTC(from),
						Utils.parseDateUTC(to)));

		assertTrue("Data must contain at least one element", !expected.isEmpty());

		// when
		final ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Diary.FindPeriod.URL)
						.queryParam(Api.Diary.FindPeriod.PARAM_FROM, from)
						.queryParam(Api.Diary.FindPeriod.PARAM_TO, to)
						.build()
				)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final List<Versioned<DiaryRecord>> actual = serializer.readAll(response);
		assertEquals(expected, actual);
	}

	@Test
	public void findPeriod_badRequest_missing() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.FindPeriod.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void findPeriod_badRequest_tooLong() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Diary.FindPeriod.URL)
						.queryParam(Api.Diary.FindPeriod.PARAM_FROM, Utils.buildString(20))
						.queryParam(Api.Diary.FindPeriod.PARAM_TO, Utils.buildString(20))
						.build()
				)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void findChanged() throws Exception
	{
		// given
		final int userId = getUserId();
		final Date since = new Date(0);
		final List<Versioned<DiaryRecord>> expected = DiaryLocalService.convert(
				diaryEntityRepository.findByUserIdAndTimeStampIsGreaterThanEqual(userId, since));

		// when
		final ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Diary.FindChanged.URL)
						.queryParam(Api.Diary.FindChanged.PARAM_SINCE, Utils.formatDateUTC(since))
						.build()
				)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final List<Versioned<DiaryRecord>> actual = serializer.readAll(response);
		assertEqualsSorted(expected, actual);
	}

	@Test
	public void findChanged_badRequest_missing() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.FindChanged.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void findChanged_badRequest_tooLong() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Diary.FindChanged.URL)
						.queryParam(Api.Diary.FindChanged.PARAM_SINCE, Utils.buildString(20))
						.build()
				)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void getHash_root() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.Hash.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isOk()
				.expectBody(String.class)
				.isEqualTo("c4cf3a3f3c8bc70ff308f963e002339d");
	}

	@Test
	public void getHash_prefix() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.Hash.URL + "/f9")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isOk()
				.expectBody(String.class)
				.isEqualTo("42b7225c5d4c862c475570308c5fbb68");
	}

	@Test
	public void getHash_badRequest_tooLong() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get()
				.uri(URL_ROOT + Api.Diary.Hash.URL + "/" + Utils.buildString(33))
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void getHashChildren() throws Exception
	{
		// given
		final Map<String, String> expected = new HashMap<String, String>()
		{{
			put("5", "821818e3ef4f41e3bcb3893364b38835");
			put("f", "42b7225c5d4c862c475570308c5fbb68");
		}};

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.Hashes.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final Map<String, String> actual = serializerMap.read(response);
		assertEquals(expected, actual);
	}

	@Test
	public void getHashChildren_prefix() throws Exception
	{
		// given
		final String prefix = "f9";
		final Map<String, String> expected = new HashMap<String, String>()
		{{
			put(prefix + "0", "7c47e50757a14528a2b0b6896cb03432");
			put(prefix + "4", "d6704d5506ab4104a5a5cab720af8736");
		}};

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Diary.Hashes.URL + "/" + prefix)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final Map<String, String> actual = serializerMap.read(response);
		assertEquals(expected, actual);
	}

	@Test
	public void getHashChildren_badRequest_tooLong() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get()
				.uri(URL_ROOT + Api.Diary.Hashes.URL + "/" + Utils.buildString(4))
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void save() throws Exception
	{
		// given
		final int userId = getUserId();
		final String itemId = "4ff091c13bf540fdaaa25b46c470e713";
		final Versioned<DiaryRecord> item = new Versioned<DiaryRecord>()
		{{
			setId(itemId);
			setTimeStamp(new Date());
			setHash("0f027e975c2944f083a038f0c4d283ec");
			setVersion(13);
			setDeleted(false);
			setData(new NoteRecord()
			{{
				setTime(new Date());
				setText("Apple");
			}});
		}};
		final List<Versioned<DiaryRecord>> data = Collections.singletonList(item);

		// when
		final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
		params.add(Api.Diary.Save.PARAM_DATA, serializer.writeAll(data));

		final ResponseSpec result = webClient
				.put().uri(URL_ROOT + Api.Diary.Save.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.body(BodyInserters.fromFormData(params))
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isOk()
				.expectBody(String.class)
				.isEqualTo(Api.Diary.Save.RESPONSE_OK);

		final Versioned<DiaryRecord> actual = DiaryLocalService.convert(
				diaryEntityRepository.findByUserIdAndId(userId, itemId));
		assertEquals(item, actual);
	}

	@Test
	public void save_badRequest_missing() throws Exception
	{
		// given / when
		final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();

		final ResponseSpec result = webClient
				.post().uri(URL_ROOT + Api.Diary.Save.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.body(BodyInserters.fromFormData(params))
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}
}
