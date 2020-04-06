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
package org.bosik.diacomp.web.backend.features.log;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Date;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
@Service
public class LogService
{
	@Value("${app.logerrors}")
	private boolean loggingEnabled;

	@Autowired
	private LogEntryRepository logEntryRepository;

	public void saveError(HttpServletRequest request, Exception e)
	{
		if (!loggingEnabled)
		{
			log.info("Logging skipped");
			return;
		}

		try
		{
			logEntryRepository.save(LogEntry.builder()
					.time(new Date())
					.remoteUser(request.getRemoteUser())
					.remoteAddress(request.getRemoteAddr())
					.requestUrl(request.getMethod() + " " + request.getRequestURI())
					.requestParams(StringUtils.abbreviate(mapToString(request.getParameterMap()), 4000))
					.errorMessage(StringUtils.abbreviate(e.getMessage(), 255))
					.stacktrace(StringUtils.abbreviate(ExceptionUtils.getStackTrace(e), 4000))
					.build()
			);
		}
		catch (Exception e2)
		{
			log.error("Failed to store log entry", e2);
		}
	}

	private static String mapToString(Map<String, String[]> map)
	{
		return map.entrySet()
				.stream()
				.map(e -> e.getKey() + "=\"" + Arrays.toString(e.getValue()) + "\"")
				.collect(Collectors.joining(", "));
	}
}
