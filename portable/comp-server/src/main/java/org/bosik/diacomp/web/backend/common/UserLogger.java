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
