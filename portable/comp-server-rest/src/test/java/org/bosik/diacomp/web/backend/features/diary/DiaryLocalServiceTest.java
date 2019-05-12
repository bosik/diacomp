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

import org.bosik.diacomp.core.utils.Utils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Collections;
import java.util.Date;
import java.util.TimeZone;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@RunWith(SpringRunner.class)
public class DiaryLocalServiceTest
{
	@MockBean
	private DiaryEntityRepository repository;

	@MockBean
	private CachedDiaryHashTree cachedHashTree;

	@Autowired
	private DiaryLocalService service;

	@Test
	public void export()
	{
		// given
		final int USER_ID = 42;
		final String VALUE_ID = "8a33b4148228541398fafa736ac2244e";
		final String VALUE_TIMESTAMP = "2019-02-19 19:58:59";
		final String VALUE_HASH = "a4eae8125496ffa44030caf006b20ab9";
		final int VALUE_VERSION = 12;
		final boolean VALUE_DELETED = false;
		final String VALUE_TIME_CACHE = "2019-02-19 19:58:52";
		final String VALUE_CONTENT = "(content)";

		final DiaryEntity entity = new DiaryEntity()
		{{
			setUserId(USER_ID);
			setId(VALUE_ID);
			setTimeStamp(Utils.parseTimeUTC(VALUE_TIMESTAMP));
			setHash(VALUE_HASH);
			setVersion(VALUE_VERSION);
			setDeleted(VALUE_DELETED);
			setContent(VALUE_CONTENT);
			setTimeCache(Utils.parseTimeUTC(VALUE_TIME_CACHE));
		}};

		when(repository.findByUserId(eq(USER_ID))).thenReturn(Collections.singletonList(entity));

		final String EXPECTED =
				"VERSION=6\n" + VALUE_TIME_CACHE + '\t' + VALUE_ID + '\t' + VALUE_TIMESTAMP + '\t' + VALUE_HASH + '\t' + String
						.valueOf(VALUE_VERSION) + '\t' + String.valueOf(VALUE_DELETED) + '\t' + VALUE_CONTENT + '\n';

		TimeZone.setDefault(TimeZone.getTimeZone("GMT+02"));

		// when
		final String actual = service.exportPlain(USER_ID);

		// then
		final Date now = new Date();
		final String timeUTC = Utils.formatTimeUTC(now);
		final String timeLocal = Utils.formatTimeLocal(TimeZone.getDefault(), now);
		assertNotEquals("This test must be run on a non-UTC time zone", timeUTC, timeLocal);

		assertEquals(EXPECTED, actual);
	}

	@TestConfiguration
	static class Config
	{
		@Bean
		public DiaryLocalService employeeService(DiaryEntityRepository repository, CachedDiaryHashTree cachedHashTree)
		{
			return new DiaryLocalService(repository, cachedHashTree);
		}
	}
}
