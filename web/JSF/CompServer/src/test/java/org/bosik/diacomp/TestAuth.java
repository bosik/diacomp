package org.bosik.diacomp;

import static org.junit.Assert.assertEquals;
import java.net.URI;
import javax.ws.rs.client.Invocation.Builder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;
import org.bosik.diacomp.resources.CompServerApp;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.JerseyClient;
import org.glassfish.jersey.client.JerseyClientBuilder;
import org.glassfish.jersey.test.JerseyTest;
import org.junit.Test;

public class TestAuth extends JerseyTest
{
	private static final String	demoLogin	= "admin";
	private static final String	demoPass	= "1234";

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
		logoutTest();

		final String url = "/api/diary/view";
		Response response = target().path(url).request(MediaType.APPLICATION_JSON).get(Response.class);
		System.out.println("unauthTest(): " + response);

		assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
	}

	@Test
	public void logoutTest()
	{
		final String url = "/api/auth/logout";
		Response response = target().path(url).request(MediaType.APPLICATION_JSON).get(Response.class);
		System.out.println("logoutTest(): " + response);

		// assertEquals(resp, "")
	}

	@Test
	public void loginTest()
	{
		final String url = "/api/auth/login";
		final Builder request = target().path(url).queryParam("login", demoLogin).queryParam("pass", demoPass)
				.request(MediaType.APPLICATION_JSON);
		String response = request.post(null, String.class);

		System.out.println("loginTest(): " + response);
	}

	@Test
	public void loginGETTest()
	{
		final String url = "/api/auth/login";
		final Builder request = target().path(url).queryParam("login", demoLogin).queryParam("pass", demoPass)
				.request(MediaType.APPLICATION_JSON);
		Response response = request.get(Response.class);

		System.out.println("loginGETTest(): " + response);
	}
}
