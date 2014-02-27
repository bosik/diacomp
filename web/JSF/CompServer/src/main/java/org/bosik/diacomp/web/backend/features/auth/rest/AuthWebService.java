package org.bosik.diacomp.web.backend.features.auth.rest;

import javax.ws.rs.core.MediaType;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.web.backend.features.common.WebService;
import org.bosik.diacomp.web.backend.utils.StdResponse;
import com.sun.jersey.api.client.UniformInterfaceException;
import com.sun.jersey.api.client.WebResource;

public class AuthWebService extends WebService implements AuthService
{
	@Override
	public void login(String login, String pass, int apiVersion)
	{
		WebResource resource = getResource("api/auth/login");
		try
		{
			resource = resource.queryParam("login", login);
			resource = resource.queryParam("pass", pass);
			resource = resource.queryParam("api", String.valueOf(apiVersion));
			String str = resource.accept(MediaType.APPLICATION_JSON).post(String.class);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException("URL: " + resource.getURI(), e);
		}
	}

	@Override
	public void logout()
	{
		WebResource resource = getResource("api/auth/logout");
		try
		{
			String str = resource.accept(MediaType.APPLICATION_JSON).get(String.class);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException("URL: " + resource.getURI(), e);
		}
	}
}
