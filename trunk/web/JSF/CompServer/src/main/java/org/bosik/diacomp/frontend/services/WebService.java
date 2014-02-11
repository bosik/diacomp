package org.bosik.diacomp.frontend.services;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.client.apache.ApacheHttpClient;
import com.sun.jersey.client.apache.config.ApacheHttpClientConfig;
import com.sun.jersey.client.apache.config.DefaultApacheHttpClientConfig;

public class WebService
{
	private static ApacheHttpClient	client;
	private String					BASE_URL;

	{
		ApacheHttpClientConfig config = new DefaultApacheHttpClientConfig();
		config.getProperties().put(ApacheHttpClientConfig.PROPERTY_HANDLE_COOKIES, true);
		client = ApacheHttpClient.create(config);
	}

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
			pro.load(is);
			BASE_URL = pro.getProperty("baseUrl");
			is.close();

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

	public static Client getClient()
	{
		return client;
	}
}
