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

	/**
	 * Scope: frontend
	 * 
	 * @return
	 */
	public static String getBaseURL()
	{
		return get("baseUrl");
	}

	/**
	 * Scope: test
	 * 
	 * @return
	 */
	public static String getTestLogin()
	{
		return get("login");
	}

	/**
	 * Scope: test
	 * 
	 * @return
	 */
	public static String getTestPassword()
	{
		return get("pass");
	}

	/**
	 * Scope: backend
	 * 
	 * @return
	 */
	public static int getAPICurrent()
	{
		return Integer.parseInt(get("current_api"));
	}

	/**
	 * Scope: backend
	 * 
	 * @return
	 */
	public static int getAPISupported()
	{
		return Integer.parseInt(get("supported_api"));
	}
}
