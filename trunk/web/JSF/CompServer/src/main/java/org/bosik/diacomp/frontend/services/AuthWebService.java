package org.bosik.diacomp.frontend.services;

import javax.ws.rs.core.MediaType;

import org.bosik.diacomp.services.AuthService;
import org.bosik.diacomp.services.exceptions.CommonServiceException;
import org.bosik.diacomp.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.services.exceptions.UnsupportedAPIException;
import org.bosik.diacomp.utils.ResponseBuilder;
import org.bosik.diacomp.utils.StdResponse;

import com.sun.jersey.api.client.WebResource;

public class AuthWebService extends WebService implements AuthService
{
	@Override
	public void login(String login, String pass, int apiVersion)
	{
		WebResource resource = getClient().resource(getBaseUrl() + "api/auth/login");
		resource = resource.queryParam("login", login);
		resource = resource.queryParam("pass", pass);
		resource = resource.queryParam("api", String.valueOf(apiVersion));
		String str = resource.accept(MediaType.APPLICATION_JSON).post(String.class);

		StdResponse stdResp = new StdResponse(str);

		switch (stdResp.getCode())
		{
			case ResponseBuilder.CODE_OK:
				return;
			case ResponseBuilder.CODE_UNAUTHORIZED:
				throw new NotAuthorizedException(stdResp.getResponse());
			case ResponseBuilder.CODE_UNSUPPORTED_API:
				throw new UnsupportedAPIException(stdResp.getResponse());
			case ResponseBuilder.CODE_DEPRECATED_API:
				throw new DeprecatedAPIException(stdResp.getResponse());
			default: // case ResponseBuilder.CODE_FAIL:
				throw new CommonServiceException(stdResp.getResponse());
		}
	}

	@Override
	public void logout()
	{
		WebResource resource = getClient().resource(getBaseUrl() + "api/auth/logout");
		String str = resource.accept(MediaType.APPLICATION_JSON).get(String.class);

		StdResponse resp = new StdResponse(str);

		switch (resp.getCode())
		{
			case ResponseBuilder.CODE_OK:
				return;
			default: // case ResponseBuilder.CODE_FAIL:
				throw new CommonServiceException(resp.getResponse());
		}
	}
}
