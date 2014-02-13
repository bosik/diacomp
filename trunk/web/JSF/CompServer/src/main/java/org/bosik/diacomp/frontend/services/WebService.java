package org.bosik.diacomp.frontend.services;

import org.bosik.diacomp.utils.MiscUtils;

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

		BASE_URL = MiscUtils.loadBaseUrl();
	}

	public WebService()
	{
		System.out.println(getClass().getSimpleName() + " loaded with URL " + BASE_URL);
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
