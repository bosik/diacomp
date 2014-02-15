package org.bosik.diacomp.features.auth.dao;

import javax.servlet.http.HttpServletRequest;

import org.bosik.diacomp.services.exceptions.NotAuthorizedException;

public interface AuthDAO
{
	// TODO: seems bad approach
	void checkAuth(HttpServletRequest request) throws NotAuthorizedException;

	int getCurrentUserId(HttpServletRequest request);

	void login(HttpServletRequest request, String login, String pass);

	void logout(HttpServletRequest request);
}
