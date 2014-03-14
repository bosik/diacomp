package org.bosik.diacomp.web.frontend.common;

import javax.ws.rs.core.MediaType;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import com.sun.jersey.api.representation.Form;

public class AuthorizedRestClient extends RestClient
{
	private final AuthService	authService;
	private final String		login;
	private final String		pass;
	private final int			apiVersion;

	private void login()
	{
		authService.login(login, pass, apiVersion);
	}

	public AuthorizedRestClient(AuthService authService, String login, String pass, int apiVersion)
	{
		this.authService = authService;
		this.login = login;
		this.pass = pass;
		this.apiVersion = apiVersion;
	}

	public String authGet(String url)
	{
		try
		{
			return getResource(url).accept(MediaType.APPLICATION_JSON).get(String.class);
		}
		catch (NotAuthorizedException e)
		{
			login();
			return getResource(url).accept(MediaType.APPLICATION_JSON).get(String.class);
		}
	}

	public String authPut(String url, Form form)
	{
		try
		{
			return getResource(url).accept(MediaType.APPLICATION_JSON).put(String.class, form);
		}
		catch (NotAuthorizedException e)
		{
			login();
			return getResource(url).accept(MediaType.APPLICATION_JSON).put(String.class, form);
		}
	}

}
