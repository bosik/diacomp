package org.bosik.diacomp.web.backend.features.auth;

import static org.junit.Assert.assertEquals;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.auth.function.AuthDAO;
import org.bosik.diacomp.web.backend.features.auth.function.MySQLAuthDAO;
import org.junit.Test;

public class TestMySQLAuthDAO
{
	private final AuthDAO	authDao	= new MySQLAuthDAO();

	@Test
	public void test()
	{
		final String login = Config.getTestLogin();
		final String pass = Config.getTestPassword();

		int id = authDao.login(login, pass);
		assertEquals(1, id);
	}
}
