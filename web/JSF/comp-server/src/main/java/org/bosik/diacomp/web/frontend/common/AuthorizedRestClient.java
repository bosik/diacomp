package org.bosik.diacomp.web.frontend.common;

import javax.ws.rs.core.MediaType;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import com.sun.jersey.api.client.WebResource;
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

	public String authGet(WebResource resource)
	{
		try
		{
			String s = resource.accept(MediaType.APPLICATION_JSON).get(String.class);
			checkResponse(new StdResponse(s));
			return s;
		}
		catch (NotAuthorizedException e)
		{
			login();

			String s = resource.accept(MediaType.APPLICATION_JSON).get(String.class);
			checkResponse(new StdResponse(s));
			return s;
		}
	}

	public String authPost(WebResource resource, Form form)
	{
		try
		{
			String s = resource.accept(MediaType.APPLICATION_JSON).post(String.class, form);
			checkResponse(new StdResponse(s));
			return s;
		}
		catch (NotAuthorizedException e)
		{
			login();

			String s = resource.accept(MediaType.APPLICATION_JSON).post(String.class, form);
			checkResponse(new StdResponse(s));
			return s;
		}
	}

	public String authPut(WebResource resource, Form form)
	{
		try
		{
			String s = resource.accept(MediaType.APPLICATION_JSON).put(String.class, form);
			checkResponse(new StdResponse(s));
			return s;
		}
		catch (NotAuthorizedException e)
		{
			login();

			String s = resource.accept(MediaType.APPLICATION_JSON).put(String.class, form);
			checkResponse(new StdResponse(s));
			return s;
		}
	}

}
