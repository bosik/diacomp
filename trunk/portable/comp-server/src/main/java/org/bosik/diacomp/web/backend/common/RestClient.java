package org.bosik.diacomp.web.backend.common;

@Deprecated
public class RestClient //implements Serializable
{
	//	private static final long		serialVersionUID	= 1L;
	//
	//	private static ApacheHttpClient	client;
	//	{
	//		initClient();
	//	}
	//
	//	public static void initClient()
	//	{
	//		if (client == null)
	//		{
	//			ApacheHttpClientConfig config = new DefaultApacheHttpClientConfig();
	//			config.getProperties().put(ApacheHttpClientConfig.PROPERTY_HANDLE_COOKIES, true);
	//			client = ApacheHttpClient.create(config);
	//		}
	//	}
	//
	//	protected static WebResource getResource(String url)
	//	{
	//		initClient();
	//		return client.resource(Config.getBaseURL() + url);
	//	}
	//
	//	protected static void checkResponse(StdResponse resp) throws CommonServiceException
	//	{
	//		switch (resp.getCode())
	//		{
	//			case ResponseBuilder.CODE_OK:
	//				return;
	//			case ResponseBuilder.CODE_NOTFOUND:
	//				return;
	//			case ResponseBuilder.CODE_UNAUTHORIZED:
	//				throw new NotAuthorizedException(resp.getResponse());
	//			case ResponseBuilder.CODE_BADCREDENTIALS:
	//				throw new NotAuthorizedException(resp.getResponse());
	//			case ResponseBuilder.CODE_UNSUPPORTED_API:
	//				throw new UnsupportedAPIException(resp.getResponse());
	//			case ResponseBuilder.CODE_DEPRECATED_API:
	//				throw new DeprecatedAPIException(resp.getResponse());
	//			default: // case ResponseBuilder.CODE_FAIL:
	//				throw new CommonServiceException("#" + resp.getCode() + ": " + resp.getResponse());
	//		}
	//	}
	//
	//	protected void handleUniformInterfaceException(UniformInterfaceException e) throws CommonServiceException
	//	{
	//		if (e.getResponse().getStatus() == Status.UNAUTHORIZED.getStatusCode())
	//		{
	//			throw new NotAuthorizedException(e);
	//		}
	//		else
	//		{
	//			throw new CommonServiceException(e);
	//		}
	//	}
}
