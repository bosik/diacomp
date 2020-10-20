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

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

@RestController
@RequestMapping("/windows")
public class WindowsAppRest
{
	private static final int MAX_FILE_NAME_LENGTH = 64;

	@Autowired
	private WindowsService service;

	@GetMapping(path = "/version", produces = MediaType.TEXT_PLAIN)
	public String getVersion() throws IOException
	{
		return String.valueOf(service.getVersionCode());
	}

	@GetMapping(path = "/file/{fileName}", produces = MediaType.APPLICATION_OCTET_STREAM)
	public ResponseEntity<?> getFile(@PathVariable("fileName") String fileName) throws FileNotFoundException
	{
		if (fileName.length() > MAX_FILE_NAME_LENGTH)
		{
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("File name too long");
		}

		try
		{
			final InputStream inputStream = service.getFileStream(fileName);
			return ResponseEntity.ok()
					.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + fileName + "\"")
					.body(new InputStreamResource(inputStream));
		}
		catch (FileNotFoundException e)
		{
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body("File not found: " + fileName);
		}
	}
}
