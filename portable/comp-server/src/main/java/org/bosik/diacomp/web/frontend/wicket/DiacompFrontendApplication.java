package org.bosik.diacomp.web.frontend.wicket;

import org.bosik.diacomp.web.backend.features.user.auth.AuthProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

@SpringBootApplication
@ComponentScan(basePackages = "org.bosik.diacomp")
@EntityScan(basePackages = "org.bosik.diacomp")
@EnableJpaRepositories("org.bosik.diacomp")
public class DiacompFrontendApplication extends SpringBootServletInitializer
{
	@Autowired
	private AuthProvider authProvider;

	public static void main(String[] args)
	{
		SpringApplication.run(DiacompFrontendApplication.class, args);
	}

	@Override
	protected SpringApplicationBuilder configure(SpringApplicationBuilder application)
	{
		return application.sources(DiacompFrontendApplication.class);
	}

	@Configuration
	public class SecurityConfiguration extends WebSecurityConfigurerAdapter
	{
		@Override
		protected void configure(HttpSecurity http) throws Exception
		{
			// @formatter:off
			http.authorizeRequests()
				.antMatchers(
						"/",
						"/api/**", // managed by REST service
						"/about",
						"/login**",
						"/register**",
						"/restore",
						"/legal/**",
						"/wicket/resource/**",
						"/res/**",
						"/favicon.ico"
				).permitAll()
				.anyRequest().authenticated()
				.and()
					.csrf().disable()
			.authenticationProvider(authProvider)
			.anonymous().authorities("ROLE_ANONYMOUS").and()
//			.formLogin()
//				.loginProcessingUrl("/login")
//				.loginPage("/login")
//				.failureForwardUrl("/login?error")
//				.defaultSuccessUrl("/diary")
			;
			// @formatter:on
		}
	}
}