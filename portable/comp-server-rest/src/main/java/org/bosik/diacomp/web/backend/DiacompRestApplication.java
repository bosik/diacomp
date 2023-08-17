package org.bosik.diacomp.web.backend;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

import java.util.Locale;

@EnableScheduling
@SpringBootApplication
public class DiacompRestApplication extends SpringBootServletInitializer
{
	public static void main(String[] args)
	{
		Locale.setDefault(Locale.ENGLISH);
		SpringApplication.run(DiacompRestApplication.class, args);
	}

	@Override
	protected SpringApplicationBuilder configure(SpringApplicationBuilder application)
	{
		return application.sources(DiacompRestApplication.class);
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