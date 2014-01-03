package org.bosik.diacomp;

import static org.junit.Assert.fail;
import java.net.URI;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.client.Invocation.Builder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.bosik.diacomp.resources.CompServerApp;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.JerseyClient;
import org.glassfish.jersey.client.JerseyClientBuilder;
import org.glassfish.jersey.test.JerseyTest;
import org.junit.Test;

public class TestAuth extends JerseyTest
{
	public TestAuth() throws Exception
	{
		super(CompServerApp.class);
	}

	private static URI getBaseURI()
	{
		return UriBuilder.fromUri("http://localhost:8082/CompServer").build();
	}

	@Override
	public WebTarget target()
	{
		ClientConfig config = new ClientConfig();
		JerseyClient client = JerseyClientBuilder.createClient(config);
		return client.target(getBaseURI());
	}

	@Test
	public void unauthTest()
	{
		try
		{
			String resp = target().path("/api/diary").request(MediaType.APPLICATION_JSON).get(String.class);
			System.out.println(resp);
			fail("Authentification broken");
		}
		catch (NotAuthorizedException e)
		{
		}
	}

	@Test
	public void logoutTest()
	{
		final String url = "/api/auth/logout";
		Response resp = target().path(url).request(MediaType.APPLICATION_JSON).get(Response.class);
		System.out.println(resp);

		// assertEquals(resp, "")
	}

	@Test
	public void loginTest()
	{
		final String url = "/api/auth/login_post";
		final String demoLogin = "admin";
		final String demoPass = "1234";

		final Builder request = target().path(url).queryParam("login", demoLogin).queryParam("pass", demoPass)
				.request(MediaType.APPLICATION_JSON);
		String response = request.post(null, String.class);

		System.out.println(response);
	}
}
