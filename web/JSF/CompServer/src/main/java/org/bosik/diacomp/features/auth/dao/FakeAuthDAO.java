package org.bosik.diacomp.features.auth.dao;

import org.bosik.diacomp.core.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.UnsupportedAPIException;

public class FakeAuthDAO implements AuthDAO
{
	private static final int	API_CURRENT	= 20;
	private static final int	API_LEGACY	= 19;

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

		if ("admin".equals(login) && "1234".equals(pass))
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
