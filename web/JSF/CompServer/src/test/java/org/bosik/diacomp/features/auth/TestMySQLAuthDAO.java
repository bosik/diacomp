package org.bosik.diacomp.features.auth;

import static org.junit.Assert.assertEquals;
import org.bosik.diacomp.web.backend.features.auth.function.AuthDAO;
import org.bosik.diacomp.web.backend.features.auth.function.MySQLAuthDAO;
import org.bosik.diacomp.web.backend.utils.Config;
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
		// assertEquals("4761fe12f59bb38d3faca218bcffdaa3", MySQLAuthDAO.md5("devel0pment"));

		final String login = Config.getLogin();
		final String pass = Config.getPassword();
		final int apiVersion = Integer.parseInt(Config.get("current_api"));

		int id = authDao.login(login, pass, apiVersion);
		assertEquals(1, id);
	}
}
