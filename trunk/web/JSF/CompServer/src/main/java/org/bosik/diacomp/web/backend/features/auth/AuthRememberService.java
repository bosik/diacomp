package org.bosik.diacomp.web.backend.features.auth;

import org.bosik.diacomp.core.services.AuthService;

public class AuthRememberService implements AuthService
{
	private AuthService	authService;
	private String		login;
	private String		pass;
	private int			apiVersion;

	public AuthRememberService(AuthService authService, String login, String pass, int apiVersion)
	{
		this.authService = authService;
		this.login = login;
		this.pass = pass;
		this.apiVersion = apiVersion;
	}

	public void login()
	{
		authService.login(login, pass, apiVersion);
	}

	@Override
	public void login(String login, String pass, int apiVersion)
	{
		authService.login(login, pass, apiVersion);
	}

	@Override
	public void logout()
	{
		authService.logout();
	}
}
