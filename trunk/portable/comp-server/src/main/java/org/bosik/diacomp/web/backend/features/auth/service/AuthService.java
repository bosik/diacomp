package org.bosik.diacomp.web.backend.features.auth.service;

import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;

public interface AuthService
{
	/**
	 * Returns current user's name if logged in, throws exception otherwise
	 * 
	 * @return
	 */
	String getCurrentUserName() throws NotAuthorizedException;

	/**
	 * Returns current user's ID if logged in, throws exception otherwise
	 * 
	 * @return
	 */
	int getCurrentUserId() throws NotAuthorizedException;
}
