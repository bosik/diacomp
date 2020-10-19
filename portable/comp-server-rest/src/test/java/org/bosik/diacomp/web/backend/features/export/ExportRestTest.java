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
package org.bosik.diacomp.web.backend.features.export;

import org.apache.commons.lang3.StringUtils;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.parsers.ParserPreferenceEntry;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.rest.ExportAPI;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.core.utils.ZipUtils;
import org.bosik.diacomp.web.backend.common.IntegrationTest;
import org.bosik.diacomp.web.backend.features.base.dish.DishBaseLocalService;
import org.bosik.diacomp.web.backend.features.base.dish.DishEntityRepository;
import org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService;
import org.bosik.diacomp.web.backend.features.base.food.user.FoodUserEntityRepository;
import org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService;
import org.bosik.diacomp.web.backend.features.diary.DiaryEntityRepository;
import org.bosik.diacomp.web.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.web.backend.features.preferences.PreferenceEntityRepository;
import org.bosik.diacomp.web.backend.features.preferences.PreferencesLocalService;
import org.bosik.merklesync.Versioned;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.junit.Assert.assertEquals;

public class ExportRestTest extends IntegrationTest
{
	@Autowired
	private DiaryEntityRepository diaryEntityRepository;

	@Autowired
	private FoodUserEntityRepository foodUserEntityRepository;

	@Autowired
	private DishEntityRepository dishEntityRepository;

	@Autowired
	private PreferenceEntityRepository preferenceEntityRepository;

	@Before
	public void onBefore()
	{
		super.onBefore();
	}

	@Test
	public void exportAsJson() throws Exception
	{
		verifyExport(
				URL_ROOT + Api.Json.URL,
				e -> parseDiaryRecordsJson(new String(e.get(ExportAPI.JSON_DIARY))),
				e -> parseFoodRecordsJson(new String(e.get(ExportAPI.JSON_FOODBASE))),
				e -> parseDishRecordsJson(new String(e.get(ExportAPI.JSON_DISHBASE))),
				e -> parsePreferencesJson(new String(e.get(ExportAPI.JSON_PREFERENCES)))
		);
	}

	@Test
	public void exportAsPlain() throws Exception
	{
		verifyExport(
				URL_ROOT + Api.Plain.URL,
				e -> parseDiaryRecordsPlain(new String(e.get(ExportAPI.PLAIN_DIARY))),
				e -> parseFoodRecordsPlain(new String(e.get(ExportAPI.PLAIN_FOODBASE))),
				e -> parseDishRecordsPlain(new String(e.get(ExportAPI.PLAIN_DISHBASE))),
				e -> parsePreferencesPlain(new String(e.get(ExportAPI.PLAIN_PREFERENCES)))
		);
	}

	private static List<Versioned<DiaryRecord>> parseDiaryRecordsJson(String s)
	{
		return new SerializerAdapter<>(new ParserVersioned<>(new ParserDiaryRecord())).readAll(s);
	}

	private static List<Versioned<FoodItem>> parseFoodRecordsJson(String s)
	{
		return new SerializerAdapter<>(new ParserVersioned<>(new ParserFoodItem())).readAll(s);
	}

	private static List<Versioned<DishItem>> parseDishRecordsJson(String s)
	{
		return new SerializerAdapter<>(new ParserVersioned<>(new ParserDishItem())).readAll(s);
	}

	private static List<PreferenceEntry<String>> parsePreferencesJson(String s)
	{
		return new SerializerAdapter<>(new ParserPreferenceEntry()).readAll(s);
	}

	// -------------------

	private static <T> List<T> parsePlain(String s, String header, Function<String, T> parser)
	{
		if (!s.startsWith(header + "\n"))
		{
			throw new IllegalArgumentException("Unsupported version");
		}

		return Arrays.stream(s.split("\n"))
				.skip(1) // header
				.filter(line -> !StringUtils.isBlank(line))
				.map(parser)
				.collect(toList());
	}

	private static <T> List<Versioned<T>> parsePlainVersioned(String s, String header, Serializer<T> parser)
	{
		return parsePlain(s, header, line ->
		{
			final String[] row = line.split("\t");

			final Versioned<T> e = new Versioned<>();
			e.setId(row[1]);
			e.setTimeStamp(Utils.parseTimeUTC(row[2]));
			e.setHash(row[3]);
			e.setVersion(Integer.parseInt(row[4]));
			e.setDeleted(Boolean.parseBoolean(row[5]));
			e.setData(parser.read(row[6]));

			return e;
		});
	}

	private static List<Versioned<DiaryRecord>> parseDiaryRecordsPlain(String s)
	{
		return parsePlainVersioned(s, "VERSION=6", new SerializerAdapter<>(new ParserDiaryRecord()));
	}

	private static List<Versioned<FoodItem>> parseFoodRecordsPlain(String s)
	{
		return parsePlainVersioned(s, "VERSION=1", new SerializerAdapter<>(new ParserFoodItem()));
	}

	private static List<Versioned<DishItem>> parseDishRecordsPlain(String s)
	{
		return parsePlainVersioned(s, "VERSION=1", new SerializerAdapter<>(new ParserDishItem()));
	}

	private static List<PreferenceEntry<String>> parsePreferencesPlain(String s)
	{
		return parsePlain(s, "VERSION=1", line ->
		{
			final String[] row = line.split("\t");

			final PreferenceEntry<String> e = new PreferenceEntry<>();

			e.setId(PreferenceID.parse(row[0]));
			e.setVersion(Integer.parseInt(row[1]));
			e.setValue(row[2]);

			return e;
		});
	}

	private void verifyExport(String exportUrl, Function<Map<String, byte[]>, List<Versioned<DiaryRecord>>> parserDiary,
			Function<Map<String, byte[]>, List<Versioned<FoodItem>>> parserFood,
			Function<Map<String, byte[]>, List<Versioned<DishItem>>> parserDish,
			Function<Map<String, byte[]>, List<PreferenceEntry<String>>> parserPreferences) throws IOException
	{
		// given
		final int userId = getUserId();

		final List<Versioned<DiaryRecord>> expectedDiary = diaryEntityRepository.findByUserId(userId).stream()
				.map(DiaryLocalService::convert)
				.collect(Collectors.toList());

		final Map<String, Versioned<FoodItem>> expectedFood = new HashMap<>();
		foodCommonEntityRepository.findAll().forEach(e -> expectedFood.put(
				e.getId(),
				FoodCommonLocalService.convert(e)
		));
		foodUserEntityRepository.findByKeyUserId(userId).forEach(e -> expectedFood.put(
				e.getKey().getId(),
				FoodUserLocalService.convert(e)
		));

		final List<Versioned<DishItem>> expectedDish = dishEntityRepository.findByUserId(userId).stream()
				.map(DishBaseLocalService::convert)
				.collect(Collectors.toList());

		final List<PreferenceEntry<String>> expectedPreferences = preferenceEntityRepository.findByIdUserId(userId).stream()
				.map(PreferencesLocalService::convert)
				.collect(Collectors.toList());

		// when
		final WebTestClient.ResponseSpec result = webClient
				.get().uri(exportUrl)
				.cookies(c -> c.addAll(signIn()))
				.exchange();

		// then
		final byte[] responseBytes = result
				.expectStatus().isOk()
				.expectBody(byte[].class)
				.returnResult()
				.getResponseBody();

		final List<ZipUtils.Entry> response = ZipUtils.unzip(new ByteArrayInputStream(responseBytes));
		final Map<String, byte[]> entries = response.stream()
				.collect(toMap(
						ZipUtils.Entry::getName,
						ZipUtils.Entry::getContent
				));

		final List<Versioned<DiaryRecord>> actualDiary = parserDiary.apply(entries);
		final List<Versioned<FoodItem>> actualFood = parserFood.apply(entries);
		final List<Versioned<DishItem>> actualDish = parserDish.apply(entries);
		final List<PreferenceEntry<String>> actualPreferences = parserPreferences.apply(entries);

		assertEqualsSorted(expectedDiary, actualDiary);
		assertEqualsSorted(expectedFood.values(), actualFood);
		assertEqualsSorted(expectedDish, actualDish);
		assertEquals(expectedPreferences, actualPreferences);
	}
}
