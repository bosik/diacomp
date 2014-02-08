package org.bosik.diacomp.frontend.services;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class WebService
{
	private String	BASE_URL;

	public WebService(String url)
	{
		BASE_URL = url;
	}

	public WebService()
	{
		try
		{
			Properties pro = new Properties();

			ClassLoader classloader = Thread.currentThread().getContextClassLoader();
			InputStream is = classloader.getResourceAsStream("config.properties");

			// InputStream is = getClass().getResourceAsStream("config.properties");
			pro.load(is);
			BASE_URL = pro.getProperty("baseUrl");

			System.out.println(getClass().getSimpleName() + " loaded with URL " + BASE_URL);
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}

	public String getBaseUrl()
	{
		return BASE_URL;
	}
}
