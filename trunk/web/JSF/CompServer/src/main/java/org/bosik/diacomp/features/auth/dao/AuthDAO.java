package org.bosik.diacomp.features.auth.dao;

import javax.servlet.http.HttpServletRequest;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;

public interface AuthDAO
{
	// TODO: seems bad approach
	void checkAuth(HttpServletRequest request) throws NotAuthorizedException;

	int getCurrentUserId(HttpServletRequest request);

	void login(HttpServletRequest request, String login, String pass, int apiVersion);

	void logout(HttpServletRequest request);
}
