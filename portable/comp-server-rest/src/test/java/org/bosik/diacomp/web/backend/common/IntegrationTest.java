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
package org.bosik.diacomp.web.backend.common;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import junitparams.JUnitParamsRunner;
import org.bosik.diacomp.web.backend.DiacompRestApplication;
import org.bosik.diacomp.web.backend.features.base.dish.DishEntity;
import org.bosik.diacomp.web.backend.features.base.dish.DishEntityRepository;
import org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonEntity;
import org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonEntityRepository;
import org.bosik.diacomp.web.backend.features.base.food.user.FoodUserEntity;
import org.bosik.diacomp.web.backend.features.base.food.user.FoodUserEntityRepository;
import org.bosik.diacomp.web.backend.features.diary.DiaryEntity;
import org.bosik.diacomp.web.backend.features.diary.DiaryEntityRepository;
import org.bosik.diacomp.web.backend.features.preferences.PreferenceEntity;
import org.bosik.diacomp.web.backend.features.preferences.PreferenceEntityRepository;
import org.bosik.diacomp.web.backend.features.user.auth.Api;
import org.bosik.diacomp.web.backend.features.user.auth.HashUtils;
import org.bosik.diacomp.web.backend.features.user.auth.UserEntity;
import org.bosik.diacomp.web.backend.features.user.auth.UserEntityRepository;
import org.bosik.merklesync.Versioned;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.rules.SpringClassRule;
import org.springframework.test.context.junit4.rules.SpringMethodRule;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.BodyInserters;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;

@RunWith(JUnitParamsRunner.class)
@SpringBootTest(
		webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
		classes = DiacompRestApplication.class
)
@AutoConfigureWebTestClient
@TestPropertySource(locations = {
		"classpath:application.properties",
		"classpath:application-test.properties"
})
public abstract class IntegrationTest
{
	@ClassRule
	public static final SpringClassRule SPRING_CLASS_RULE = new SpringClassRule();

	protected static final String URL_ROOT    = "/api";
	private static final   String API_VERSION = "20";

	@Rule
	public final SpringMethodRule springMethodRule = new SpringMethodRule();

	protected static final String USER_NAME     = "test_user";
	private static final   String USER_PASSWORD = "test_password";

	@Autowired
	protected WebTestClient webClient;

	@Autowired
	protected UserEntityRepository userEntityRepository;

	@Autowired
	private DiaryEntityRepository diaryEntityRepository;

	@Autowired
	protected FoodUserEntityRepository foodUserEntityRepository;

	@Autowired
	protected FoodCommonEntityRepository foodCommonEntityRepository;

	@Autowired
	private DishEntityRepository dishEntityRepository;

	@Autowired
	private PreferenceEntityRepository preferenceEntityRepository;

	@Autowired
	private ObjectMapper mapper;

	@Before
	public void onBefore()
	{
		final List<UserEntity> userEntities = Arrays.asList(
				UserEntity.builder()
						.id(1)
						.name(USER_NAME)
						.hashPass(HashUtils.createHash(USER_PASSWORD))
						.registrationDate(new Date())
						.build(),
				UserEntity.builder()
						.id(2)
						.name("Another user")
						.hashPass(HashUtils.createHash("Another password"))
						.registrationDate(new Date())
						.build()
		);

		userEntityRepository.deleteAll();
		final Map<Integer, Integer> mapIdUser = userEntities.stream()
				.collect(Collectors.toMap(
						e -> e.getId(),
						e -> userEntityRepository.save(e).getId()
				));

		diaryEntityRepository.deleteAll();
		loadDataDiary().stream()
				.peek(e -> e.setUserId(mapIdUser.get(e.getUserId())))
				.forEach(e -> diaryEntityRepository.save(e));

		foodCommonEntityRepository.deleteAll();
		loadDataFoodCommon()
				.forEach(e -> foodCommonEntityRepository.save(e));

		foodUserEntityRepository.deleteAll();
		loadDataFoodUser().stream()
				.peek(e -> e.getKey().setUserId(mapIdUser.get(e.getKey().getUserId())))
				.forEach(e -> foodUserEntityRepository.save(e));

		dishEntityRepository.deleteAll();
		loadDataDish().stream()
				.peek(e -> e.setUserId(mapIdUser.get(e.getUserId())))
				.forEach(e -> dishEntityRepository.save(e));

		preferenceEntityRepository.deleteAll();
		loadDataPreferences().stream()
				.peek(e -> e.getId().setUserId(mapIdUser.get(e.getId().getUserId())))
				.forEach(e -> preferenceEntityRepository.save(e));
	}

	private MultiValueMap<String, String> signIn(String userName, String password)
	{
		final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
		params.add(Api.Auth.Login.PARAM_USERNAME, userName);
		params.add(Api.Auth.Login.PARAM_PASSWORD, password);
		params.add(Api.Auth.Login.PARAM_API_VERSION, API_VERSION);

		final MultiValueMap<String, ResponseCookie> responseCookies = webClient
				.post().uri(URL_ROOT + Api.Auth.Login.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.body(BodyInserters.fromFormData(params))
				.exchange()
				.expectStatus().isOk()
				.returnResult(Object.class)
				.getResponseCookies();

		final MultiValueMap<String, String> cookies = new LinkedMultiValueMap<>();
		responseCookies.forEach((k, list) -> list.forEach(r -> cookies.add(r.getName(), r.getValue())));
		return cookies;
	}

	protected MultiValueMap<String, String> signIn()
	{
		return signIn(USER_NAME, USER_PASSWORD);
	}

	protected void signInInvalid(String userName, String password)
	{
		final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
		params.add(Api.Auth.Login.PARAM_USERNAME, userName);
		params.add(Api.Auth.Login.PARAM_PASSWORD, password);
		params.add(Api.Auth.Login.PARAM_API_VERSION, API_VERSION);

		webClient
				.post().uri(URL_ROOT + Api.Auth.Login.URL)
				.contentType(MediaType.APPLICATION_FORM_URLENCODED)
				.body(BodyInserters.fromFormData(params))
				.exchange()
				.expectStatus().isUnauthorized();
	}

	protected int getUserId()
	{
		return userEntityRepository.findByName(USER_NAME).getId();
	}

	protected static <T> void assertEqualsSorted(Collection<Versioned<T>> expected, List<Versioned<T>> actual)
	{
		assertEqualsSorted(new ArrayList<>(expected), actual);
	}

	protected static <T> void assertEqualsSorted(List<Versioned<T>> expected, List<Versioned<T>> actual)
	{
		expected.sort(Comparator.comparing(Versioned::getId));
		actual.sort(Comparator.comparing(Versioned::getId));
		assertEquals(expected, actual);
	}

	private List<DiaryEntity> loadDataDiary()
	{
		try
		{
			return loadResource("dump-diary.json", new TypeReference<List<DiaryEntity>>()
			{
			});
		}
		catch (IOException e)
		{
			throw new UncheckedIOException(e);
		}
	}

	private List<FoodCommonEntity> loadDataFoodCommon()
	{
		try
		{
			return loadResource("dump-food-common.json", new TypeReference<List<FoodCommonEntity>>()
			{
			});
		}
		catch (IOException e)
		{
			throw new UncheckedIOException(e);
		}
	}

	private List<FoodUserEntity> loadDataFoodUser()
	{
		try
		{
			return loadResource("dump-food-user.json", new TypeReference<List<FoodUserEntity>>()
			{
			});
		}
		catch (IOException e)
		{
			throw new UncheckedIOException(e);
		}
	}

	private List<DishEntity> loadDataDish()
	{
		try
		{
			return loadResource("dump-dish.json", new TypeReference<List<DishEntity>>()
			{
			});
		}
		catch (IOException e)
		{
			throw new UncheckedIOException(e);
		}
	}

	private List<PreferenceEntity> loadDataPreferences()
	{
		try
		{
			return loadResource("dump-preferences.json", new TypeReference<List<PreferenceEntity>>()
			{
			});
		}
		catch (IOException e)
		{
			throw new UncheckedIOException(e);
		}
	}

	private <T> List<T> loadResource(String resourceFileName, TypeReference<List<T>> typeRef) throws IOException
	{
		try (final InputStream inputStream = IntegrationTest.class.getResourceAsStream("/" + resourceFileName))
		{
			return mapper.readValue(inputStream, typeRef);
		}
	}
}
