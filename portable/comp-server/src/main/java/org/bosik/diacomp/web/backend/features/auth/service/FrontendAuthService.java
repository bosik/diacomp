package org.bosik.diacomp.web.backend.features.auth.service;

import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.auth.function.AuthDAO;
import org.bosik.diacomp.web.backend.features.auth.function.MySQLAuthDAO;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

public class FrontendAuthService implements AuthService
{
	private static final String	GUEST_USERNAME	= "guest";

	private final AuthDAO		authDao			= new MySQLAuthDAO();

	//private static final Map<String, Integer>	userMap			= new HashMap<String, Integer>();

	@Override
	public int getCurrentUserId()
	{
		String userName = getCurrentUserName();

		// caching
		Integer id;// = userMap.get(userName);
		//if (id == null)
		{
			id = authDao.getIdByName(userName);
			if (id == null)
			{
				throw new NotAuthorizedException(String.format("User %s is not authorized", userName));
			}
			//userMap.put(userName, id);
		}

		return id;
	}

	@Override
	public String getCurrentUserName()
	{
		SecurityContext context = SecurityContextHolder.getContext();
		Authentication auth = context.getAuthentication();

		if (auth == null)
		{
			throw new NotAuthorizedException();
		}

		String userName = auth.getName();
		if (userName.equals(GUEST_USERNAME))
		{
			throw new NotAuthorizedException();
		}

		return userName;
	}
}
