package org.bosik.diacomp.web.backend.features.auth.service;

import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.auth.function.AuthService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

@Service
public class UserInfoServiceImpl implements UserInfoService
{
	private static final String	GUEST_USERNAME	= "guest";

	@Autowired
	private AuthService			authService;

	//private static final Map<String, Integer>	userMap			= new HashMap<String, Integer>();

	@Override
	public int getCurrentUserId()
	{
		String userName = getCurrentUserName();

		// caching
		Integer id;// = userMap.get(userName);
		//if (id == null)
		{
			id = authService.getIdByName(userName);
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
