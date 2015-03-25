package org.bosik.diacomp.web.backend.features.user.auth;

import org.bosik.diacomp.core.services.exceptions.AuthException;

public interface AuthService
{
	/**
	 * Registers new inactive account
	 * 
	 * @param userName
	 * @param password
	 * @return Activation key
	 */
	String register(String userName, String password);

	/**
	 * Activates user's account
	 * 
	 * @param activationKey
	 * @return userId
	 */
	int activate(String activationKey);

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

	/**
	 * Converts user ID to user name
	 * 
	 * @param userId
	 * @return User name if found, null otherwise
	 */
	String getNameById(int userId);
}
