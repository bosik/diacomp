package org.bosik.diacomp.web.backend.features.auth.function;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.core.services.exceptions.AuthException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.AuthenticationProvider;
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
			authService.login(email, password);

			List<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
			authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
			return new UsernamePasswordAuthenticationToken(email, password, authorities);
		}
		catch (AuthException e)
		{
			return null;
		}
	}

	@Override
	public boolean supports(Class<?> authentication)
	{
		return authentication.equals(UsernamePasswordAuthenticationToken.class);
	}
}
