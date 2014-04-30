package org.bosik.diacomp.web.frontend.features.auth;

import javax.ws.rs.core.MediaType;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.web.frontend.common.RestClient;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import com.sun.jersey.api.client.UniformInterfaceException;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.representation.Form;

public class AuthRestClient extends RestClient implements AuthService
{
	private static final long	serialVersionUID	= 1L;

	@Override
	public void login(String login, String pass, int apiVersion)
	{
		WebResource resource = getResource("api/auth/login");
		try
		{
			Form form = new Form();
			form.add("login", login);
			form.add("pass", pass);
			form.add("api", String.valueOf(apiVersion));
			String str = resource.accept(MediaType.APPLICATION_JSON).post(String.class, form);

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

	@Override
	public String getUserName()
	{
		SecurityContext context = SecurityContextHolder.getContext();
		Authentication auth = context.getAuthentication();
		String userName = auth.getName();
		return userName.equals("guest") ? null : userName;
	}
}
