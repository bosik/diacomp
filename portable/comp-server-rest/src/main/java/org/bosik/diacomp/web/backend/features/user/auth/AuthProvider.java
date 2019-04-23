/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.user.auth;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.core.services.exceptions.AuthException;
import org.bosik.diacomp.core.services.exceptions.NotActivatedException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

@Component
public class AuthProvider implements AuthenticationProvider
{
	@Autowired
	private AuthService	authService;

	@Override
	public Authentication authenticate(Authentication authentication) throws AuthenticationException
	{
		try
		{
			String email = authentication.getName();
			String password = authentication.getCredentials().toString();
			int userId = authService.login(email, password);
			String userInfo = String.format("%d:%s", userId, email);

			List<GrantedAuthority> authorities = new ArrayList<>();
			authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
			return new UsernamePasswordAuthenticationToken(userInfo, password, authorities);
		}
		catch (NotActivatedException e)
		{
			// TODO: do something specific
			throw new BadCredentialsException("Not activated", e);
		}
		catch (AuthException e)
		{
			throw new BadCredentialsException("Unauthorized", e);
		}
	}

	@Override
	public boolean supports(Class<?> authentication)
	{
		return authentication.equals(UsernamePasswordAuthenticationToken.class);
	}
}
