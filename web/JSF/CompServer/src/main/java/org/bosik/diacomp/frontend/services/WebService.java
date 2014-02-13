package org.bosik.diacomp.frontend.services;

import org.bosik.diacomp.services.exceptions.CommonServiceException;
import org.bosik.diacomp.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.services.exceptions.UnsupportedAPIException;
import org.bosik.diacomp.utils.MiscUtils;
import org.bosik.diacomp.utils.ResponseBuilder;
import org.bosik.diacomp.utils.StdResponse;

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

	protected static void checkResponse(StdResponse resp) throws CommonServiceException
	{
		switch (resp.getCode())
		{
			case ResponseBuilder.CODE_OK:
				return;
			case ResponseBuilder.CODE_UNAUTHORIZED:
				throw new NotAuthorizedException(resp.getResponse());
			case ResponseBuilder.CODE_UNSUPPORTED_API:
				throw new UnsupportedAPIException(resp.getResponse());
			case ResponseBuilder.CODE_DEPRECATED_API:
				throw new DeprecatedAPIException(resp.getResponse());
			default: // case ResponseBuilder.CODE_FAIL:
				throw new CommonServiceException(resp.getResponse());
		}
	}
}
