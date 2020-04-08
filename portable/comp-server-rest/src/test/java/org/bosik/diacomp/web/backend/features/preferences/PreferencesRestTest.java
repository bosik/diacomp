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

import org.bosik.diacomp.core.entities.business.Units;
import org.bosik.diacomp.core.persistence.parsers.ParserPreferenceEntry;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.diary.MealFormat;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.web.backend.common.IntegrationTest;
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
import java.util.List;

import static org.junit.Assert.assertEquals;

public class PreferencesRestTest extends IntegrationTest
{
	private final Serializer<PreferenceEntry<String>> serializer = new SerializerAdapter<>(new ParserPreferenceEntry());

	@Autowired
	private PreferenceEntityRepository preferenceEntityRepository;

	@Before
	public void onBefore()
	{
		super.onBefore();

		preferenceEntityRepository.deleteAll();
		preferenceEntityRepository.saveAll(buildData(getUserId()));
	}

	private static List<PreferenceEntity> buildData(int userId)
	{
		return Arrays.asList(
				PreferenceEntity.builder()
						.id(PreferenceEntityPK.builder()
								.userId(userId)
								.key(PreferenceID.RATES_MASS_UNITS.getCode())
								.build())
						.value(Units.Mass.BU.getCode())
						.version(4)
						.build(),
				PreferenceEntity.builder()
						.id(PreferenceEntityPK.builder()
								.userId(userId)
								.key(PreferenceID.ANDROID_MEAL_FORMAT.getCode())
								.build())
						.value(MealFormat.LIST_SORTED_BY_CARBS.getCode())
						.version(20)
						.build(),
				PreferenceEntity.builder()
						.id(PreferenceEntityPK.builder()
								.userId(userId + 1) // another one
								.key(PreferenceID.RATES_MASS_UNITS.getCode())
								.build())
						.value(Units.Mass.G.getCode())
						.version(1)
						.build()
		);
	}

	@Test
	public void getAll() throws Exception
	{
		// given
		final List<PreferenceEntry<String>> expected = PreferencesLocalService.convert(
				preferenceEntityRepository.findByIdUserId(getUserId()));

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Preferences.GetAll.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final List<PreferenceEntry<String>> actual = serializer.readAll(response);
		assertEquals(expected, actual);
	}

	@Test
	public void getString() throws Exception
	{
		// given
		final PreferenceEntry<String> expected = PreferencesLocalService.convert(
				preferenceEntityRepository.findByIdUserId(getUserId())).get(0);

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Preferences.GetString.URL + "/" + expected.getId().getCode())
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final String response = result
				.expectStatus().isOk()
				.expectBody(String.class)
				.returnResult()
				.getResponseBody();

		final PreferenceEntry<String> actual = serializer.read(response);
		assertEquals(expected, actual);
	}

	@Test
	public void getString_badRequest() throws Exception
	{
		// given / when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Preferences.GetString.URL + "/123456")
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result.expectStatus().isBadRequest();
	}

	@Test
	public void getHash() throws Exception
	{
		// given
		final String hash = "1105";

		// when
		final ResponseSpec result = webClient
				.get().uri(URL_ROOT + Api.Preferences.Hash.URL)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isOk()
				.expectBody(String.class)
				.isEqualTo(hash);
	}

	@Test
	public void save() throws Exception
	{
		// given
		final PreferenceEntry<String> expected = new PreferenceEntry<>();
		expected.setId(PreferenceID.ANDROID_MEAL_FORMAT);
		expected.setValue(MealFormat.TOTAL_CARBS_BU.getCode());
		expected.setVersion(9);

		final List<PreferenceEntry<String>> data = Collections.singletonList(expected);

		// when
		final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
		params.add(Api.Preferences.Save.PARAM_DATA, serializer.writeAll(data));

		final ResponseSpec result = webClient
				.put().uri(URL_ROOT + Api.Preferences.Save.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.body(BodyInserters.fromFormData(params))
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		result
				.expectStatus().isOk()
				.expectBody(String.class)
				.isEqualTo(Api.Preferences.Save.RESPONSE_OK);

		final PreferenceEntry<String> actual = PreferencesLocalService.convert(
				preferenceEntityRepository.findById(new PreferenceEntityPK(getUserId(), expected.getKey())).get());
		assertEquals(expected, actual);
	}
}
