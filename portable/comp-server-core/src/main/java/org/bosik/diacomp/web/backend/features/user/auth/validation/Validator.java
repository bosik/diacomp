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
package org.bosik.diacomp.web.backend.features.user.auth.validation;

import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordIsEmptyException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordTooLongException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordTooShortException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameIsEmptyException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameTooLongException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameTooShortException;

public class Validator
{
	private static final int USERNAME_MIN_SIZE = 8;
	private static final int USERNAME_MAX_SIZE = 50;
	private static final int PASSWORD_MIN_SIZE = 6;
	private static final int PASSWORD_MAX_SIZE = 64;

	public static void validateUserName(String userName)
	{
		if (userName == null || userName.isEmpty())
		{
			throw new UserNameIsEmptyException("User name can't be empty");
		}

		if (userName.length() < USERNAME_MIN_SIZE)
		{
			throw new UserNameTooShortException("User name is too short, must be at least " + USERNAME_MIN_SIZE + " chars long",
					USERNAME_MIN_SIZE);
		}

		if (userName.length() > USERNAME_MAX_SIZE)
		{
			throw new UserNameTooLongException("User name too long, must be at most " + USERNAME_MAX_SIZE + " chars long",
					USERNAME_MAX_SIZE);
		}
	}

	public static void validatePassword(String password)
	{
		if (password == null || password.isEmpty())
		{
			throw new PasswordIsEmptyException("Password can't be empty");
		}

		if (password.length() < PASSWORD_MIN_SIZE)
		{
			throw new PasswordTooShortException("Password is too short, must be at least " + PASSWORD_MIN_SIZE + " chars long",
					PASSWORD_MIN_SIZE);
		}

		if (password.length() > PASSWORD_MAX_SIZE)
		{
			throw new PasswordTooLongException("Password is too long, must be at most " + PASSWORD_MAX_SIZE + " chars long",
					PASSWORD_MAX_SIZE);
		}
	}
}
