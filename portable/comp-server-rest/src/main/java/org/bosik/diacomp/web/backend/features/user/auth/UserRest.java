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

import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class UserRest
{
	//	@Autowired
	//	private TokenService tokenService;
	//
	//	@HeaderParam("Authorization")
	//	private String auth;

	@Autowired
	private UserInfoService userInfoService;

	protected int getUserId() throws NotAuthorizedException
	{
		return userInfoService.getCurrentUserId();

		//		if (auth == null || auth.isEmpty() || !auth.startsWith("Bearer ") || auth.length() > 512)
		//		{
		//			throw new NotAuthorizedException();
		//		}
		//
		//		return tokenService.getUserId(auth.substring(7));
	}
}
