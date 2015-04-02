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
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

// TODO: separate basic Config class from concrete implementations
public class Config
{
	private static Map<String, String>	props;

	public static synchronized void init()
	{
		if (props == null)
		{
			Properties properties = new Properties();

			ClassLoader classloader = Thread.currentThread().getContextClassLoader();
			InputStream is = classloader.getResourceAsStream("config.properties");
			try
			{
				properties.load(is);
				is.close();

				props = new HashMap<String, String>();
				for (Entry<Object, Object> entry : properties.entrySet())
				{
					final String key = (String)entry.getKey();
					final String value = (String)entry.getValue();
					props.put(key, value);
				}
			}
			catch (IOException e)
			{
				throw new RuntimeException(e);
			}

			System.out.println("Config loaded OK");
		}
	}

	public static String get(String key)
	{
		init();

		String value = props.get(key);

		if (null == value)
		{
			throw new RuntimeException(String.format("Config property '%s' not found", key));
		}
		else
		{
			return value;
		}
	}

	public static int getInt(String key)
	{
		return Integer.parseInt(get(key));
	}
}
