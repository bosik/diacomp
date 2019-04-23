package org.bosik.diacomp.web.backend.common;

import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.springframework.http.HttpStatus;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

@ControllerAdvice
public class RestExceptionAdvice
{
	@ResponseBody
	@ExceptionHandler(IllegalArgumentException.class)
	@ResponseStatus(HttpStatus.BAD_REQUEST)
	public String handlerIllegalArgumentException(IllegalArgumentException e)
	{
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
	@ExceptionHandler(Exception.class)
	@ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
	public String handlerException(Exception e)
	{
		e.printStackTrace(); // FIXME: Use proper logger
		return ResponseBuilder.buildFails();
	}
}
