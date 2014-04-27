package org.bosik.diacomp.web.frontend.common;

import java.io.Serializable;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.UnsupportedAPIException;
import org.bosik.diacomp.web.backend.common.Config;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.client.apache.ApacheHttpClient;
import com.sun.jersey.client.apache.config.ApacheHttpClientConfig;
import com.sun.jersey.client.apache.config.DefaultApacheHttpClientConfig;

public class RestClient implements Serializable
{
	private static final long		serialVersionUID	= 1L;

	private static ApacheHttpClient	client;
	{
		initClient();
	}

	public static void initClient()
	{
		if (client == null)
		{
			ApacheHttpClientConfig config = new DefaultApacheHttpClientConfig();
			config.getProperties().put(ApacheHttpClientConfig.PROPERTY_HANDLE_COOKIES, true);
			client = ApacheHttpClient.create(config);
		}
	}

	protected static WebResource getResource(String url)
	{
		initClient();
		return client.resource(Config.getBaseURL() + url);
	}

	protected static void checkResponse(StdResponse resp) throws CommonServiceException
	{
		switch (resp.getCode())
		{
			case ResponseBuilder.CODE_OK:
				return;
			case ResponseBuilder.CODE_NOTFOUND:
				return;
			case ResponseBuilder.CODE_UNAUTHORIZED:
				throw new NotAuthorizedException(resp.getResponse());
			case ResponseBuilder.CODE_BADCREDENTIALS:
				throw new NotAuthorizedException(resp.getResponse());
			case ResponseBuilder.CODE_UNSUPPORTED_API:
				throw new UnsupportedAPIException(resp.getResponse());
			case ResponseBuilder.CODE_DEPRECATED_API:
				throw new DeprecatedAPIException(resp.getResponse());
			default: // case ResponseBuilder.CODE_FAIL:
				throw new CommonServiceException("#" + resp.getCode() + ": " + resp.getResponse());
		}
	}
}
