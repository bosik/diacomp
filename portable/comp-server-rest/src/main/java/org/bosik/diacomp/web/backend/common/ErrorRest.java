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

import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;

@RestController
public class ErrorRest implements ErrorController
{
	private static final String PATH = "/error";

	@RequestMapping(value = PATH)
	public String error(HttpServletRequest request)
	{
		Object status = request.getAttribute(RequestDispatcher.ERROR_STATUS_CODE);

		if (status != null && status instanceof Integer)
		{
			Integer statusCode = (Integer) status;

			if (statusCode == HttpStatus.NOT_FOUND.value())
			{
				return "Not Found";
			}
			else if (statusCode == HttpStatus.INTERNAL_SERVER_ERROR.value())
			{
				return "Internal Server Error";
			}
		}

		return "Unknown Error";
	}

	@Override
	public String getErrorPath()
	{
		return PATH;
	}
}
