package org.bosik.diacomp.frontend.services;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import org.bosik.diacomp.services.AuthService;
import org.bosik.diacomp.services.exceptions.CommonServiceException;
import org.bosik.diacomp.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.services.exceptions.UnsupportedAPIException;
import org.bosik.diacomp.utils.ResponseBuilder;
import org.bosik.diacomp.utils.StdResponse;

public class AuthWebService extends WebService implements AuthService
{
	@Override
	public void login(String login, String pass, int apiVersion)
	{
		Client client = ClientBuilder.newClient();
		WebTarget webTarget = client.target(getBaseUrl() + "auth/login");
		webTarget = webTarget.queryParam("login", login);
		webTarget = webTarget.queryParam("pass", pass);
		webTarget = webTarget.queryParam("api", apiVersion);
		String s = webTarget.request(MediaType.APPLICATION_JSON).get(String.class);

		StdResponse resp = new StdResponse(s);

		switch (resp.getCode())
		{
			case ResponseBuilder.CODE_OK:
				return;
			case ResponseBuilder.CODE_UNAUTHORIZED:
				throw new NotAuthorizedException(resp.getMsg());
			case ResponseBuilder.CODE_UNSUPPORTED_API:
				throw new UnsupportedAPIException(resp.getMsg());
			case ResponseBuilder.CODE_DEPRECATED_API:
				throw new DeprecatedAPIException(resp.getMsg());
			default: // case ResponseBuilder.CODE_FAIL:
				throw new CommonServiceException(resp.getMsg());
		}
	}

	@Override
	public void logout()
	{
		Client client = ClientBuilder.newClient();
		WebTarget webTarget = client.target(getBaseUrl() + "auth/logout");
		String s = webTarget.request(MediaType.APPLICATION_JSON).get(String.class);

		StdResponse resp = new StdResponse(s);

		switch (resp.getCode())
		{
			case ResponseBuilder.CODE_OK:
				return;
			default: // case ResponseBuilder.CODE_FAIL:
				throw new CommonServiceException(resp.getMsg());
		}

	}

}
