package org.bosik.diacomp.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class MiscUtils
{
	public static String loadBaseUrl()
	{
		try
		{
			Properties properties = new Properties();
			ClassLoader classloader = Thread.currentThread().getContextClassLoader();
			InputStream is = classloader.getResourceAsStream("config.properties");
			try
			{
				properties.load(is);
			}
			finally
			{
				is.close();
			}
			return properties.getProperty("baseUrl");
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}
