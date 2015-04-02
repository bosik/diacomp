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
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

@Service
public class UserInfoServiceImpl implements UserInfoService
{
	private static final String	GUEST_USERNAME	= "guest";

	@Autowired
	private AuthService			authService;

	@Override
	public int getCurrentUserId()
	{
		String id = getUserInfo()[0];
		return Integer.parseInt(id);
	}

	@Override
	public String getCurrentUserName()
	{
		return getUserInfo()[1];
	}

	private static String[] getUserInfo()
	{
		SecurityContext context = SecurityContextHolder.getContext();
		Authentication auth = context.getAuthentication();

		if (auth == null)
		{
			throw new NotAuthorizedException();
		}

		String userInfo = auth.getName();
		if (userInfo.equals(GUEST_USERNAME))
		{
			throw new NotAuthorizedException();
		}

		String[] items = userInfo.split(":");
		if (items.length != 2)
		{
			throw new NotAuthorizedException("Invalid userinfo format: " + userInfo);
		}

		return items;
	}
}
