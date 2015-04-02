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
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("fake")
public class FakeAuthService implements AuthService
{
	private static final String	FAKE_USERNAME	= "admin";
	private static final String	FAKE_PASSWORD	= "password";
	private static final int	FAKE_USER_ID	= 1;

	@Override
	public int login(String login, String pass)
	{
		if (FAKE_USERNAME.equals(login) && FAKE_PASSWORD.equals(pass))
		{
			return FAKE_USER_ID;
		}
		else
		{
			throw new NotAuthorizedException();
		}
	}

	@Override
	public Integer getIdByName(String userName)
	{
		if (FAKE_USERNAME.equals(userName))
		{
			return FAKE_USER_ID;
		}
		else
		{
			return null;
		}
	}

	@Override
	public int activate(String activationKey)
	{
		return FAKE_USER_ID;
	}

	@Override
	public String register(String userName, String password)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getNameById(int userId)
	{
		// TODO Auto-generated method stub
		return null;
	}
}
