/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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

	String buildRestoreKey(String userName);

	void changePassword(String restoreKey, String newPassword);
}
