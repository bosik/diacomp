package org.bosik.diacomp.web.backend;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

@SpringBootApplication
public class DiacompRestApplication
{
	public static void main(String[] args)
	{
		SpringApplication.run(DiacompRestApplication.class, args);
	}

	@Configuration
	public class SecurityConfiguration extends WebSecurityConfigurerAdapter
	{
		@Override
		protected void configure(HttpSecurity http) throws Exception
		{
			// @formatter:off
			http.authorizeRequests()
				.antMatchers("/system/**", "/auth/**", "/windows/**", "/").permitAll()
				//.anyRequest().authenticated() // not compatible with Windows client
				.and()
				.csrf().disable()
				//.httpBasic() // not compatible with Windows client
			;
			// @formatter:on
		}
	}
}