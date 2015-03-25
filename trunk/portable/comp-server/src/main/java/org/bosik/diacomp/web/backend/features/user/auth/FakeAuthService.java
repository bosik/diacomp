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
