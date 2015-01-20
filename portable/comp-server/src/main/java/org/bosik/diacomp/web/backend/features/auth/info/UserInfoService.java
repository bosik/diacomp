package org.bosik.diacomp.web.backend.features.auth.info;

import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;

public interface UserInfoService
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
