package org.bosik.diacomp.frontend.services;

import static org.junit.Assert.assertEquals;

import java.net.URI;

import javax.ws.rs.client.Invocation.Builder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;

import org.bosik.diacomp.backend.resources.CompServerApp;
import org.bosik.diacomp.utils.ResponseBuilder;
import org.bosik.diacomp.utils.StdResponse;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.JerseyClient;
import org.glassfish.jersey.client.JerseyClientBuilder;
import org.glassfish.jersey.test.JerseyTest;
import org.junit.Test;

import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.client.apache.ApacheHttpClient;
import com.sun.jersey.client.apache.config.ApacheHttpClientConfig;
import com.sun.jersey.client.apache.config.DefaultApacheHttpClientConfig;

public class TestAuthResource extends JerseyTest
{
	// FIXME
	private static final String	BASE_URL	= "http://localhost:8090/CompServer";

	private static final int	API_VERSION	= 20;
	private static final String	demoLogin	= "admin";
	private static final String	demoPass	= "1234";

	public TestAuthResource() throws Exception
	{
		super(CompServerApp.class);
	}

	private static URI getBaseURI()
	{
		return UriBuilder.fromUri(BASE_URL).build();
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
		String response = null;
		// try
		// {
		response = target().path(url).request(MediaType.APPLICATION_JSON).get(String.class);
		// fail("NotAuthorizedException was not thrown");
		// }
		// catch (NotAuthorizedException e)
		// {
		// it's right
		// }

		System.out.println("unauthTest(): " + response);

		// assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
		StdResponse r = StdResponse.decode(response);
		assertEquals(ResponseBuilder.CODE_UNAUTHORIZED, r.getCode());
	}

	@Test
	public void logoutTest()
	{
		final String url = "/api/auth/logout";
		String response = target().path(url).request(MediaType.APPLICATION_JSON).get(String.class);
		System.out.println("logoutTest(): " + response);

		StdResponse r = StdResponse.decode(response.toString());
		assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	}

	@Test
	public void loginTest_correct()
	{
		final String url = "/api/auth/login";
		final Builder request = target().path(url).queryParam("login", demoLogin).queryParam("pass", demoPass)
				.queryParam("api", API_VERSION).request(MediaType.APPLICATION_JSON);
		String response = request.post(null, String.class);

		System.out.println("loginTest(): " + response);

		StdResponse r = StdResponse.decode(response.toString());
		assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	}

	@Test
	public void loginTest_wrong()
	{
		final String url = "/api/auth/login";
		final Builder request = target().path(url).queryParam("login", demoLogin).queryParam("pass", "wrong")
				.queryParam("api", API_VERSION).request(MediaType.APPLICATION_JSON);
		String response = request.post(null, String.class);

		System.out.println("loginTest(): " + response);

		StdResponse r = StdResponse.decode(response.toString());
		assertEquals(ResponseBuilder.CODE_BADCREDENTIALS, r.getCode());
	}

	@Test
	public void loginGETTest()
	{
		final String url = "/api/auth/login";
		final Builder request = target().path(url).queryParam("login", demoLogin).queryParam("pass", demoPass)
				.queryParam("api", API_VERSION).request(MediaType.APPLICATION_JSON);
		String response = request.get(String.class);

		System.out.println("loginGETTest(): " + response);
		StdResponse r = StdResponse.decode(response.toString());
		assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	}

	@Test
	public void doubleCheck()
	{
		ApacheHttpClientConfig config = new DefaultApacheHttpClientConfig();
		config.getProperties().put(ApacheHttpClientConfig.PROPERTY_HANDLE_COOKIES, true);
		ApacheHttpClient client = ApacheHttpClient.create(config);

		WebResource resource = client.resource(BASE_URL + "/api/auth/login").queryParam("login", demoLogin)
				.queryParam("pass", demoPass).queryParam("api", "20");
		String resp = resource.post(String.class);

		System.out.println("doubleCheck: resp[1] = " + resp);

		//=============================================================

		resource = client.resource(BASE_URL + "/api/diary/new").queryParam("mod_after", "2012-01-01%2012:15:42");
		resp = resource.get(String.class);

		System.out.println("doubleCheck: resp[2] = " + resp);

	}
}
