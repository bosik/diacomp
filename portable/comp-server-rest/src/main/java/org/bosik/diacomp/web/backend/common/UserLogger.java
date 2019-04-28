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

import org.apache.log4j.FileAppender;
import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class UserLogger
{
	@Autowired
	private UserInfoService	userInfoService;

	public Logger getLogger()
	{
		String name;

		try
		{
			name = String.format("user_%d", userInfoService.getCurrentUserId());
		}
		catch (NotAuthorizedException e)
		{
			name = "anon";
		}

		Logger logger = LogManager.exists(name);
		if (logger == null)
		{
			logger = LogManager.getLogger(name);

			FileAppender appender = new FileAppender();
			appender.setFile("logs/" + name + "/log.txt");
			appender.setLayout(new PatternLayout("%d %-5p [%C{1}] %m%n"));
			appender.setThreshold(Level.DEBUG);
			appender.setAppend(true);
			appender.activateOptions();

			logger.addAppender(appender);
		}
		return logger;
	}
}
