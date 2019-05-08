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

import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

@Service
public class WindowsService
{
	private static final String DOWNLOAD_FOLDER = "download/win32/";
	private static final String FILE_VERSION    = "version.txt";

	private List<String> files = getFilesList(DOWNLOAD_FOLDER);

	private static List<String> getFilesList(String path)
	{
		final File[] files = new File(path).listFiles();

		// null returned if the path is invalid
		if (files != null)
		{
			return Stream.of(files).filter(e -> !e.isDirectory()).map(File::getName).collect(toList());
		}
		else
		{
			return new ArrayList<>();
		}
	}

	public String getVersionInfo() throws IOException
	{
		return new String(Files.readAllBytes(Paths.get(DOWNLOAD_FOLDER, FILE_VERSION)));
	}

	public InputStream getFileStream(String fileName) throws FileNotFoundException
	{
		if (files.contains(fileName))
		{
			return new FileInputStream(new File(DOWNLOAD_FOLDER + fileName));
		}
		else
		{
			throw new FileNotFoundException(fileName);
		}
	}
}
