package org.bosik.diacomp.web.backend.features.user.info;

import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
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

	@Override
	public int getCurrentUserId()
	{
		String id = getUserInfo()[0];
		return Integer.parseInt(id);
	}

	@Override
	public String getCurrentUserName()
	{
		return getUserInfo()[1];
	}

	private static String[] getUserInfo()
	{
		SecurityContext context = SecurityContextHolder.getContext();
		Authentication auth = context.getAuthentication();

		if (auth == null)
		{
			throw new NotAuthorizedException();
		}

		String userInfo = auth.getName();
		if (userInfo.equals(GUEST_USERNAME))
		{
			throw new NotAuthorizedException();
		}

		String[] items = userInfo.split(":");
		if (items.length != 2)
		{
			throw new NotAuthorizedException("Invalid userinfo format: " + userInfo);
		}

		return items;
	}
}
