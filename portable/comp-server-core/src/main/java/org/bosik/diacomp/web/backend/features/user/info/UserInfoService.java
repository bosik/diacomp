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
package org.bosik.diacomp.web.backend.features.user.info;

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
