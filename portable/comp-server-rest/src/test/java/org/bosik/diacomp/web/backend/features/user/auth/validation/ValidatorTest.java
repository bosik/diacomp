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

import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordIsEmptyException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordTooLongException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordTooShortException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameInvalidException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameIsEmptyException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameTooLongException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameTooShortException;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(JUnitParamsRunner.class)
public class ValidatorTest
{
	@Test(expected = PasswordIsEmptyException.class)
	@Parameters(method = "getEmptyStrings")
	public void validatePassword_isEmpty(String password)
	{
		Validator.validatePassword(password);
	}

	@Test(expected = PasswordTooShortException.class)
	@Parameters(value = {
			"123",
			"12345"
	})
	public void validatePassword_tooShort(String password)
	{
		Validator.validatePassword(password);
	}

	@Test
	@Parameters({
			"123456",
			"password",
			"5bd3db21f2df44a8a25d242ff69aeed4",
			"1234567890123456789012345678901234567890123456789012345678901234"
	})
	public void validatePassword_ok(String password)
	{
		Validator.validatePassword(password);
	}

	@Test(expected = PasswordTooLongException.class)
	@Parameters({
			"12345678901234567890123456789012345678901234567890123456789012345"
	})
	public void validatePassword_tooLong(String password)
	{
		Validator.validatePassword(password);
	}

	@Test(expected = UserNameIsEmptyException.class)
	@Parameters(method = "getEmptyStrings")
	public void validateUserName_isEmpty(String password)
	{
		Validator.validateUserName(password);
	}

	@Test(expected = UserNameTooShortException.class)
	@Parameters(value = {
			"1234",
			"1234567"
	})
	public void validateUserName_tooShort(String UserName)
	{
		Validator.validateUserName(UserName);
	}

	@Test
	@Parameters({
			"abc.def.123_4@gmail.com",
			"abc@domain.sub.com",
			"abc@yandex.ru"
	})
	public void validateUserName_ok(String UserName)
	{
		Validator.validateUserName(UserName);
	}

	@Test(expected = UserNameTooLongException.class)
	@Parameters({
			"123456789012345678901234567890123456789012345678901"
	})
	public void validateUserName_tooLong(String UserName)
	{
		Validator.validateUserName(UserName);
	}

	@Test(expected = UserNameInvalidException.class)
	@Parameters({
			"12345678",
			"UserName",
			"aaa@bbbb",
			"aaa@.com",
			"a#c@def.com",
			"5bd3db21f2df44a8a25d242ff69aeed4",
			"12345678901234567890123456789012345678901234567890"
	})
	public void validateUserName_invalid(String UserName)
	{
		Validator.validateUserName(UserName);
	}

	// used in tests
	private static String[] getEmptyStrings()
	{
		return new String[] { null, "" };
	}
}
