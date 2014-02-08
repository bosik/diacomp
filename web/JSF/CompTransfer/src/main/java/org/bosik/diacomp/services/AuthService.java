package org.bosik.diacomp.services;

import org.bosik.diacomp.services.exceptions.NotAuthorizedException;

public interface AuthService
{
	void login(String login, String pass, int apiVersion);

	// TODO: implement logout

	void checkAuth(Object context) throws NotAuthorizedException;
}
