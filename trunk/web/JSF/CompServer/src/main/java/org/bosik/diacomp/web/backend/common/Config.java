package org.bosik.diacomp.web.backend.common;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

public class Config
{
	private static Map<String, String>	props;

	// public static String LOGIN;
	// public static String PASSWORD;
	// public static int API_CURRENT;
	// public static int API_SUPPORTED;
	// public static String BASE_URL;

	{
		// init();

		// LOGIN = get("login");
		// PASSWORD = get("pass");
		// API_CURRENT = Integer.parseInt(get("current_api"));
		// API_SUPPORTED = Integer.parseInt(get("supported_api"));
		// BASE_URL = get("baseUrl");
	}

	public static void init()
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
					// System.out.println(String.format("[CONFIG] %s = %s", key, value));
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
		// System.out.println(String.format("Config: %s=%s", key, value));
		if (null == value)
		{
			throw new RuntimeException(String.format("Config property '%s' not found", key));
		}
		return value;
	}

	public static String getBaseURL()
	{
		return get("baseUrl");
	}

	public static String getLogin()
	{
		return get("login");
	}

	public static String getPassword()
	{
		return get("pass");
	}

	public static int getAPICurrent()
	{
		return Integer.parseInt(get("current_api"));
	}

	public static int getAPISupported()
	{
		return Integer.parseInt(get("supported_api"));
	}
}
