package org.bosik.diacomp.frontend.services;

import javax.ws.rs.core.MediaType;

import org.bosik.diacomp.services.AuthService;
import org.bosik.diacomp.services.exceptions.CommonServiceException;
import org.bosik.diacomp.utils.StdResponse;

import com.sun.jersey.api.client.UniformInterfaceException;
import com.sun.jersey.api.client.WebResource;

public class AuthWebService extends WebService implements AuthService
{
	@Override
	public void login(String login, String pass, int apiVersion)
	{
		try
		{
			WebResource resource = getResource("api/auth/login");
			resource = resource.queryParam("login", login);
			resource = resource.queryParam("pass", pass);
			resource = resource.queryParam("api", String.valueOf(apiVersion));
			String str = resource.accept(MediaType.APPLICATION_JSON).post(String.class);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void logout()
	{
		try
		{
			WebResource resource = getResource("api/auth/logout");
			String str = resource.accept(MediaType.APPLICATION_JSON).get(String.class);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException(e);
		}
	}
}
