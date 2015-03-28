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
