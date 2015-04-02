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
package org.bosik.diacomp.web.backend.features.auth;

import static org.junit.Assert.assertEquals;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TestMySQLAuthDAO
{
	@Autowired
	private AuthService	authService;

	@Test
	public void test()
	{
		final String login = Config.get("testuser.login");
		final String pass = Config.get("testuser.pass");

		int id = authService.login(login, pass);
		assertEquals(1, id);
	}
}
