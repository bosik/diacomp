package org.bosik.diacomp.web.backend.features.auth.function;

import org.bosik.diacomp.core.services.exceptions.AuthException;

public interface AuthDAO
{
	/**
	 * Validates the supplied userName:password pair
	 * 
	 * @param userName
	 * @param password
	 * @return ID of user if validation done OK
	 * @throws AuthException
	 *             If validation failed
	 */
	int login(String userName, String password) throws AuthException;

	/**
	 * Converts user name to user ID
	 * 
	 * @param userName
	 * @return ID if user found, null otherwise
	 */
	Integer getIdByName(String userName);
}
