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

import lombok.extern.slf4j.Slf4j;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.web.backend.features.log.LogService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

import javax.servlet.http.HttpServletRequest;

@Slf4j
@Service
@ControllerAdvice
public class RestExceptionAdvice
{
	@Autowired
	private LogService logService;

	@ResponseBody
	@ExceptionHandler(IllegalArgumentException.class)
	@ResponseStatus(HttpStatus.BAD_REQUEST)
	public String handlerIllegalArgumentException(HttpServletRequest request, IllegalArgumentException e)
	{
		logService.saveError(request, e);
		return e.getMessage();
	}

	@ResponseBody
	@ExceptionHandler(MissingServletRequestParameterException.class)
	@ResponseStatus(HttpStatus.BAD_REQUEST)
	public String handlerMissingServletRequestParameterException(MissingServletRequestParameterException e)
	{
		return e.getMessage();
	}

	@ResponseBody
	@ExceptionHandler(HttpRequestMethodNotSupportedException.class)
	@ResponseStatus(HttpStatus.BAD_REQUEST)
	public String handlerHttpRequestMethodNotSupportedException(HttpRequestMethodNotSupportedException e)
	{
		return e.getMessage();
	}

	@ResponseBody
	@ExceptionHandler(NotAuthorizedException.class)
	@ResponseStatus(HttpStatus.UNAUTHORIZED)
	public String handlerNotAuthorizedException(NotAuthorizedException e)
	{
		return ResponseBuilder.buildNotAuthorized();
	}

	@ResponseBody
	@ExceptionHandler(NotFoundException.class)
	@ResponseStatus(HttpStatus.NOT_FOUND)
	public String handlerNotFoundException(NotFoundException e)
	{
		return e.getMessage();
	}

	@ResponseBody
	@ExceptionHandler(Exception.class)
	@ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
	public String handlerException(HttpServletRequest request, Exception e)
	{
		logService.saveError(request, e);
		log.error(e.getMessage(), e);
		return ResponseBuilder.buildFails();
	}
}
