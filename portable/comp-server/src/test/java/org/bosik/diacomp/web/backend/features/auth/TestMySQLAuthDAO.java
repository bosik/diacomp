package org.bosik.diacomp.web.backend.features.auth;

import static org.junit.Assert.assertEquals;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.auth.function.AuthDAO;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TestMySQLAuthDAO
{
	@Autowired
	private AuthDAO	authDao;

	@Test
	public void test()
	{
		final String login = Config.get("login");
		final String pass = Config.get("pass");

		int id = authDao.login(login, pass);
		assertEquals(1, id);
	}
}
