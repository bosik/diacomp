package org.bosik.diacomp.web.backend.features.auth;

import static org.junit.Assert.assertEquals;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.auth.function.AuthDAO;
import org.bosik.diacomp.web.backend.features.auth.function.MySQLAuthDAO;
import org.junit.Test;

public class TestMySQLAuthDAO
{
	private AuthDAO	authDao	= new MySQLAuthDAO();

	{
		Config.init();
	}

	@Test
	public void test()
	{
		final String login = Config.getTestLogin();
		final String pass = Config.getTestPassword();
		final int apiVersion = Integer.parseInt(Config.get("current_api"));

		int id = authDao.login(login, pass, apiVersion);
		assertEquals(1, id);
	}
}
