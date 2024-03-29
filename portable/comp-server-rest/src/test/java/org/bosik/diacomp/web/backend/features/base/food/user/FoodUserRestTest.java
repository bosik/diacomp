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
package org.bosik.diacomp.web.backend.features.base.food.user;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class FoodUserRestTest extends IntegrationTest
{
	private final Serializer<Map<String, String>> serializerMap = new SerializerMap();
	private final Serializer<Versioned<FoodItem>> serializer    = new SerializerFoodItem();

	@Autowired
	private FoodUserEntityRepository foodUserEntityRepository;

	@Before
	public void onBefore()
	{
		super.onBefore();

		foodUserEntityRepository.deleteAll();
		foodUserEntityRepository.saveAll(buildData(getUserId()));
	}

	private static List<FoodUserEntity> buildData(int userId)
	{
		return Arrays.asList(
				FoodUserEntity.builder()
						.key(FoodUserEntityPK.builder()
								.id("f906b4341cc54a59acf23ed0108164f0")
								.userId(userId)
								.build()
						)
						.lastModified(new Date())
						.hash("7c47e50757a14528a2b0b6896cb03432")
						.version(1)
						.deleted(false)
						.name("Food 1")
						.prots(1.0)
						.fats(2.0)
						.carbs(3.0)
						.value(16.8)
						.fromTable(true)
						.build(),
				FoodUserEntity.builder()
						.key(FoodUserEntityPK.builder()
								.id("5e2d537c5110494d90d7f5277f4367ca")
								.userId(userId)
								.build()
						)
						.lastModified(new Date())
						.hash("821818e3ef4f41e3bcb3893364b38835")
						.version(1)
						.deleted(false)
						.name("Food 2")
						.prots(2.0)
						.fats(3.0)
						.carbs(4.0)
						.value(28.7)
						.fromTable(false)
						.build(),
				FoodUserEntity.builder()
						.key(FoodUserEntityPK.builder()
								.id("f9455692ef294608a85c1d7d52fb6956")
								.userId(userId)
								.build()
						)
						.lastModified(new Date())
						.hash("d6704d5506ab4104a5a5cab720af8736")
						.version(2)
						.deleted(true)
						.name("Deleted food")
						.prots(0.01)
						.fats(0.0)
						.carbs(88.0)
						.value(280)
						.fromTable(false)
						.build(),
				FoodUserEntity.builder()
						.key(FoodUserEntityPK.builder()
								.id("5e20de21655447ce849e42a1c51db615")
								.userId(userId + 1) // another user
								.build()
						)
						.lastModified(new Date())
						.hash("684ad8d6363b4c1bbdb9e74b9472adc6")
						.version(1)
						.deleted(false)
						.name("Food 4")
						.prots(4.2)
						.fats(3.5)
						.carbs(5.7)
						.value(56.2)
						.fromTable(false)
						.build()
		);
	}

	@Test
	public void count_findAll() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Food.User.Count.URL)
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
				.get().uri(URL_ROOT + Api.Food.User.Count.URL + "/5e")
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
				.uri(URL_ROOT + Api.Food.User.Count.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa")
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
		final List<Versioned<FoodItem>> expected = FoodUserLocalService.convert(foodUserEntityRepository.findByKeyUserId(userId));

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Food.User.FindById.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final List<Versioned<FoodItem>> actual = serializer.readAll(response);
		assertEqualsSorted(expected, actual);
	}

	@Test
	public void findById_prefix() throws Exception
	{
		// given
		final int userId = getUserId();
		final List<Versioned<FoodItem>> expected = FoodUserLocalService.convert(
				foodUserEntityRepository.findByKeyUserIdAndKeyIdStartingWith(userId, "5e"));

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Food.User.FindById.URL + "/5e")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final List<Versioned<FoodItem>> actual = serializer.readAll(response);
		assertEqualsSorted(expected, actual);
	}

	@Test
	public void findById_single() throws Exception
	{
		// given
		final int userId = getUserId();
		final Versioned<FoodItem> expected = FoodUserLocalService.convert(
				foodUserEntityRepository.findByKeyUserIdAndKeyId(userId, "f906b4341cc54a59acf23ed0108164f0"));

		// when
		final ResponseSpec result = webClient
				.get()
				.uri(URL_ROOT + Api.Food.User.FindById.URL + "/f906b4341cc54a59acf23ed0108164f0")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final Versioned<FoodItem> actual = serializer.read(response);
		assertEquals(expected, actual);
	}

	@Test
	public void findById_single_notFound() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get()
				.uri(URL_ROOT + Api.Food.User.FindById.URL + "/0d869e560e474f0bb5fcc46824313a62")
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
				.uri(URL_ROOT + Api.Food.User.FindById.URL + "/542a8a10ef1a41ecb9338dbeb4a931faa")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isBadRequest()
				.expectBody(String.class)
				.isEqualTo("String too long: 33 chars passed, but at most 32 are allowed");
	}

	@Test
	public void findAll_nonDeleted() throws Exception
	{
		// given
		final int userId = getUserId();
		final List<Versioned<FoodItem>> expected = FoodUserLocalService.convert(
				foodUserEntityRepository.findByKeyUserIdAndDeletedIsFalse(userId));

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Food.User.FindAll.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final List<Versioned<FoodItem>> actual = serializer.readAll(response);
		assertEqualsSorted(expected, actual);
	}

	@Test
	public void findAll_includeDeleted() throws Exception
	{
		// given
		final int userId = getUserId();
		final List<Versioned<FoodItem>> expected = FoodUserLocalService.convert(
				foodUserEntityRepository.findByKeyUserId(userId));

		// when
		final ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Food.User.FindAll.URL)
						.queryParam(Api.Food.User.FindAll.PARAM_INCLUDE_REMOVED, "true")
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

		final List<Versioned<FoodItem>> actual = serializer.readAll(response);
		assertEqualsSorted(expected, actual);
	}

	@Test
	public void findAll_badRequest_tooLong() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Food.User.FindAll.URL)
						.queryParam(Api.Food.User.FindAll.PARAM_INCLUDE_REMOVED,
								Utils.buildString(6))
						.build()
				)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void findAny() throws Exception
	{
		// given
		final int userId = getUserId();
		final String query = "od";
		final List<Versioned<FoodItem>> expected = FoodUserLocalService.convert(foodUserEntityRepository
				.findByKeyUserIdAndDeletedIsFalseAndNameContaining(
						userId, query
				));

		assertTrue("Data must contain at least one element", !expected.isEmpty());

		// when
		final ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Food.User.FindAny.URL)
						.queryParam(Api.Food.User.FindAny.PARAM_FILTER, query)
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

		final List<Versioned<FoodItem>> actual = serializer.readAll(response);
		assertEqualsSorted(expected, actual);
	}

	@Test
	public void findAny_badRequest_missing() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Food.User.FindAny.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void findAny_badRequest_tooLong() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Food.User.FindAny.URL)
						.queryParam(Api.Food.User.FindAny.PARAM_FILTER, Utils.buildString(257))
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
		final List<Versioned<FoodItem>> expected = FoodUserLocalService.convert(
				foodUserEntityRepository.findByKeyUserIdAndLastModifiedIsGreaterThanEqual(userId, since));

		// when
		final ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Food.User.FindChanged.URL)
						.queryParam(Api.Food.User.FindChanged.PARAM_SINCE,
								Utils.formatDateUTC(since))
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

		final List<Versioned<FoodItem>> actual = serializer.readAll(response);
		assertEqualsSorted(expected, actual);
	}

	@Test
	public void findChanged_badRequest_missing() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Food.User.FindChanged.URL)
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
				.get().uri(u -> u.path(URL_ROOT + Api.Food.User.FindChanged.URL)
						.queryParam(Api.Food.User.FindChanged.PARAM_SINCE,
								Utils.buildString(20))
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
				.get().uri(URL_ROOT + Api.Food.User.Hash.URL)
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
				.get().uri(URL_ROOT + Api.Food.User.Hash.URL + "/f9")
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
				.get().uri(URL_ROOT + Api.Food.User.Hash.URL + "/" + Utils.buildString(33))
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
				.get().uri(URL_ROOT + Api.Food.User.Hashes.URL)
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
				.get().uri(URL_ROOT + Api.Food.User.Hashes.URL + "/" + prefix)
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
				.get().uri(URL_ROOT + Api.Food.User.Hashes.URL + "/" + Utils.buildString(4))
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
		final Versioned<FoodItem> item = new Versioned<FoodItem>()
		{{
			setId(itemId);
			setTimeStamp(new Date());
			setHash("0f027e975c2944f083a038f0c4d283ec");
			setVersion(13);
			setDeleted(false);
			setData(new FoodItem()
			{{
				setName("Apple");
				setRelProts(0.2);
				setRelFats(0.1);
				setRelCarbs(11.2);
				setRelValue(40);
			}});
		}};
		final List<Versioned<FoodItem>> data = Collections.singletonList(item);

		// when
		final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
		params.add(Api.Food.User.Save.PARAM_DATA, serializer.writeAll(data));

		final ResponseSpec result = webClient
				.put().uri(URL_ROOT + Api.Food.User.Save.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.body(BodyInserters.fromFormData(params))
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isOk()
				.expectBody(String.class)
				.isEqualTo(Api.Food.User.Save.RESPONSE_OK);

		final Versioned<FoodItem> actual = FoodUserLocalService.convert(
				foodUserEntityRepository.findByKeyUserIdAndKeyId(userId, itemId));
		assertEquals(item, actual);
	}

	@Test
	public void save_badRequest_missing() throws Exception
	{
		// given / when
		final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();

		final ResponseSpec result = webClient
				.post().uri(URL_ROOT + Api.Food.User.Save.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.body(BodyInserters.fromFormData(params))
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void save_badRequest_longName() throws Exception
	{
		// given
		final String itemId = "d6b5c98b09a24e85a1852a9fe8a52273";
		final String longName = "01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567891";
		final Versioned<FoodItem> item = new Versioned<FoodItem>()
		{{
			setId(itemId);
			setTimeStamp(new Date());
			setHash("7686cab1275e45c2923218d920e4c774");
			setVersion(1);
			setDeleted(false);
			setData(new FoodItem()
			{{
				setName(longName);
			}});
		}};
		final List<Versioned<FoodItem>> data = Collections.singletonList(item);

		// when
		final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
		params.add(Api.Food.User.Save.PARAM_DATA, serializer.writeAll(data));

		final ResponseSpec result = webClient
				.put().uri(URL_ROOT + Api.Food.User.Save.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.body(BodyInserters.fromFormData(params))
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isBadRequest()
				.expectBody(String.class)
				.isEqualTo("Name too long, max 100 chars allowed: " + longName);
	}

	@Test
	public void save_badRequest_veryLongName() throws Exception
	{
		// given
		final String itemId = "d6b5c98b09a24e85a1852a9fe8a52273";
		final String longName = Utils.buildString(1000);
		final Versioned<FoodItem> item = new Versioned<FoodItem>()
		{{
			setId(itemId);
			setTimeStamp(new Date());
			setHash("7686cab1275e45c2923218d920e4c774");
			setVersion(1);
			setDeleted(false);
			setData(new FoodItem()
			{{
				setName(longName);
			}});
		}};
		final List<Versioned<FoodItem>> data = Collections.singletonList(item);

		// when
		final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
		params.add(Api.Food.User.Save.PARAM_DATA, serializer.writeAll(data));

		final ResponseSpec result = webClient
				.put().uri(URL_ROOT + Api.Food.User.Save.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.body(BodyInserters.fromFormData(params))
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isBadRequest()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		assertNotNull(response);
		assertTrue(response, response.startsWith("Name too long, max 100 chars allowed: "));
		assertTrue(response, response.length() < 500);
	}
}
