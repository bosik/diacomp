package org.bosik.diacomp.web.backend.features.auth.function;

import org.bosik.diacomp.core.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.UnsupportedAPIException;

public class FakeAuthDAO implements AuthDAO
{
	private static final int	API_CURRENT		= 20;
	private static final int	API_LEGACY		= 19;
	private static final String	FAKE_LOGIN		= "bosik-007@narod.ru";
	private static final String	FAKE_PASSWORD	= "devel0pment";

	@Override
	public int login(String login, String pass, int apiVersion)
	{
		if (apiVersion < API_LEGACY)
		{
			String msg = String.format("API %d is unsupported. The latest API: %d. Legacy API: %d.", apiVersion,
					API_CURRENT, API_LEGACY);
			throw new UnsupportedAPIException(msg);
		}

		if (apiVersion < API_CURRENT)
		{
			String msg = String.format(
					"API %d is still supported, but deprecated. The latest API: %d. Legacy API: %d.", apiVersion,
					API_CURRENT, API_LEGACY);
			throw new DeprecatedAPIException(msg);
		}

		if (FAKE_LOGIN.equals(login) && FAKE_PASSWORD.equals(pass))
		{
			int id = 1;
			return id;
		}
		else
		{
			throw new NotAuthorizedException();
		}
	}
}
