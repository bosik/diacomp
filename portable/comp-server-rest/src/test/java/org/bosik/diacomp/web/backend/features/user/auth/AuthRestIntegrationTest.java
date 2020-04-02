package org.bosik.diacomp.web.backend.features.user.auth;

import org.bosik.diacomp.web.backend.common.IntegrationTest;
import org.junit.Test;
import org.springframework.util.MultiValueMap;

import java.util.Date;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class AuthRestIntegrationTest extends IntegrationTest
{
	@Test
	public void anonShouldSignInAndOut() throws Exception
	{
		// 1. LOGIN

		// given
		webClient
				.get().uri(URL_ROOT + Api.Auth.Check.URL)
				.exchange()
				.expectStatus().isUnauthorized();

		// when
		final MultiValueMap<String, String> cookies = signIn();

		// then
		final Date dateSignIn = userEntityRepository.findByName(USER_NAME).getLoginDate();
		assertThat(dateSignIn).isCloseTo(new Date(), 1000L);

		webClient
				.get().uri(URL_ROOT + Api.Auth.Check.URL)
				.cookies(c -> c.addAll(cookies))
				.exchange()
				.expectStatus().isOk();

		// 2. LOGOUT

		// given / when
		webClient
				.get().uri(URL_ROOT + Api.Auth.Logout.URL)
				.cookies(c -> c.addAll(cookies))
				.exchange()
				.expectStatus().isOk();

		// then
		webClient
				.get().uri(URL_ROOT + Api.Auth.Check.URL)
				.exchange()
				.expectStatus().isUnauthorized();
	}

	@Test
	public void anonShouldNotSignIn() throws Exception
	{
		// given / when / then
		signInInvalid(USER_NAME, "bad password");
	}
}
