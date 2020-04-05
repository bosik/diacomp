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
package org.bosik.diacomp.web.backend.features.base.food.common;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.IntegrationTest;
import org.bosik.merklesync.Versioned;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

public class FoodCommonRestTest extends IntegrationTest
{
	private final Serializer<Versioned<FoodItem>> serializer = new SerializerFoodItem();

	@Autowired
	private FoodCommonEntityRepository foodCommonEntityRepository;

	@Before
	public void onBefore()
	{
		super.onBefore();

		foodCommonEntityRepository.deleteAll();
		foodCommonEntityRepository.saveAll(buildData());
	}

	private static List<FoodCommonEntity> buildData()
	{
		return Arrays.asList(
				FoodCommonEntity.builder()
						.id("f906b4341cc54a59acf23ed0108164f0")
						.lastModified(new Date())
						.hash("7c47e50757a14528a2b0b6896cb03432")
						.version(1)
						.deleted(false)
						.name("Common Food 1")
						.prots(1.0)
						.fats(2.0)
						.carbs(3.0)
						.value(16.8)
						.fromTable(true)
						.build(),
				FoodCommonEntity.builder()
						.id("5e2d537c5110494d90d7f5277f4367ca")
						.lastModified(new Date())
						.hash("821818e3ef4f41e3bcb3893364b38835")
						.version(1)
						.deleted(false)
						.name("Common Food 2")
						.prots(2.0)
						.fats(3.0)
						.carbs(4.0)
						.value(28.7)
						.fromTable(false)
						.build(),
				FoodCommonEntity.builder()
						.id("f9455692ef294608a85c1d7d52fb6956")
						.lastModified(new Date())
						.hash("d6704d5506ab4104a5a5cab720af8736")
						.version(2)
						.deleted(true)
						.name("Common Deleted food")
						.prots(0.01)
						.fats(0.0)
						.carbs(88.0)
						.value(280)
						.fromTable(false)
						.build(),
				FoodCommonEntity.builder()
						.id("5e20de21655447ce849e42a1c51db615")
						.lastModified(new Date())
						.hash("684ad8d6363b4c1bbdb9e74b9472adc6")
						.version(1)
						.deleted(false)
						.name("Common Food 4")
						.prots(4.2)
						.fats(3.5)
						.carbs(5.7)
						.value(56.2)
						.fromTable(false)
						.build()
		);
	}

	@Test
	public void find_all() throws Exception
	{
		// given
		final List<Versioned<FoodItem>> expected = FoodCommonLocalService.convert(foodCommonEntityRepository.findAll());

		// when
		final WebTestClient.ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Food.Common.FindChanged.URL)
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
	public void find_changed() throws Exception
	{
		// given
		final Date time = new Date(0);
		final List<Versioned<FoodItem>> expected = FoodCommonLocalService.convert(
				foodCommonEntityRepository.findByLastModifiedIsGreaterThanEqual(time));

		// when
		final WebTestClient.ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Food.Common.FindChanged.URL)
						.queryParam(Api.Food.Common.FindChanged.PARAM_FIND_LAST_MODIFIED, Utils.formatTimeUTC(time))
						.build()
				)
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
	public void find_changed_tooLong() throws Exception
	{
		// given / when
		final WebTestClient.ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Food.Common.FindChanged.URL)
						.queryParam(Api.Food.Common.FindChanged.PARAM_FIND_LAST_MODIFIED, Utils.buildString(20))
						.build()
				)
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	@Ignore // TODO: make use of controller advice
	public void find_changed_wayTooLong() throws Exception
	{
		// given / when
		final WebTestClient.ResponseSpec result = webClient
				.get().uri(u -> u.path(URL_ROOT + Api.Food.Common.FindChanged.URL)
						.queryParam(Api.Food.Common.FindChanged.PARAM_FIND_LAST_MODIFIED, Utils.buildString(10 * 1024 * 1024))
						.build()
				)
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}
}
