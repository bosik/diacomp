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
package org.bosik.diacomp.web.backend.features.windows;

import org.bosik.diacomp.web.backend.features.log.LogService;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(WindowsAppRest.class)
public class WindowsAppRestTest
{
	public static class Api
	{
		public static final String BASE_URL = "/windows";

		public static class Version
		{
			public static final String URL = BASE_URL + "/version";
		}

		public static class File
		{
			public static final String URL = BASE_URL + "/file/";
		}
	}

	@Autowired
	private MockMvc mvc;

	@MockBean
	private WindowsService service;

	@MockBean
	private LogService logService;

	@Test
	public void getVersion() throws Exception
	{
		// given
		final int version = 42;
		when(service.getVersionCode()).thenReturn(version);

		// when
		ResultActions request = mvc.perform(get(Api.Version.URL));

		// then
		request.andExpect(status().isOk()).andExpect(content().string(String.valueOf(version)));
	}

	@Test
	public void getFile_known() throws Exception
	{
		// given
		final String fileName = "known_file.txt";
		final InputStream stream = new ByteArrayInputStream(new byte[] {});
		when(service.getFileStream(eq(fileName))).thenReturn(stream);

		// when
		ResultActions request = mvc.perform(get(Api.File.URL + fileName));

		// then
		request.andExpect(status().isOk());
	}

	@Test
	public void getFile_unknown() throws Exception
	{
		// given
		final String fileName = "unknown_file.txt";
		when(service.getFileStream(eq(fileName))).thenThrow(new FileNotFoundException(fileName));

		// when
		ResultActions request = mvc.perform(get(Api.File.URL + fileName));

		// then
		request.andExpect(status().isNotFound());
	}
}
