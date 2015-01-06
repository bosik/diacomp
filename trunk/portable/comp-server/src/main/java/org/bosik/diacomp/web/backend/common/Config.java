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
