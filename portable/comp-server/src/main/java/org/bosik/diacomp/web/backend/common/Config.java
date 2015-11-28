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

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class Config
{
	private static Properties	properties;

	public static final String	KEY_DB_SCHEME		= "DIACOMP_DB_SCHEME";
	public static final String	KEY_DB_USER			= "DIACOMP_DB_USER";
	public static final String	KEY_DB_PASSWORD		= "DIACOMP_DB_PASSWORD";
	public static final String	KEY_EMAIL_SERVER	= "DIACOMP_EMAIL_SERVER";
	public static final String	KEY_EMAIL_LOGIN		= "DIACOMP_EMAIL_LOGIN";
	public static final String	KEY_EMAIL_PASSWORD	= "DIACOMP_EMAIL_PASSWORD";
	public static final String	KEY_CAPTCHA_SECRET	= "DIACOMP_CAPTCHA_SECRET";
	public static final String	KEY_TEST_LOGIN		= "DIACOMP_TESTUSER_LOGIN";
	public static final String	KEY_TEST_PASSWORD	= "DIACOMP_TESTUSER_PASSWORD";

	static
	{
		properties = new Properties();

		ClassLoader classloader = Thread.currentThread().getContextClassLoader();
		InputStream is = classloader.getResourceAsStream("config.properties");
		try
		{
			properties.load(is);
			is.close();
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}

		System.out.println("Config loaded OK");
	}

	public static String get(String key)
	{
		String value = System.getenv(key);
		if (value != null)
		{
			return value;
		}

		value = properties.getProperty(key);
		if (value != null)
		{
			return value;
		}

		throw new RuntimeException(String.format("Config property '%s' not found", key));
	}

	public static int getInt(String key)
	{
		return Integer.parseInt(get(key));
	}
}
