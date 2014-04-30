package org.bosik.diacomp.web.backend.features.auth.function;

import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;

public class FakeAuthDAO implements AuthDAO
{
	private static final String	FAKE_USERNAME	= "bosik-007@narod.ru";
	private static final String	FAKE_PASSWORD	= "devel0pment";
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
}
