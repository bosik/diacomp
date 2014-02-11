package org.bosik.diacomp.frontend.services;

import static org.junit.Assert.assertEquals;
import java.net.URI;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;
import org.bosik.diacomp.utils.ResponseBuilder;
import org.bosik.diacomp.utils.StdResponse;
import org.junit.Test;
import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.client.apache.ApacheHttpClient;
import com.sun.jersey.client.apache.config.ApacheHttpClientConfig;
import com.sun.jersey.client.apache.config.DefaultApacheHttpClientConfig;

public class TestAuthResource
{
	// FIXME
	private static final String	BASE_URL	= "http://localhost:8082/CompServer";

	private static final String	API_VERSION	= "20";								// TODO: make it
																					// integer
	private static final String	demoLogin	= "admin";
	private static final String	demoPass	= "1234";

	private static URI getBaseURI()
	{
		return UriBuilder.fromUri(BASE_URL).build();
	}

	private static Client getClient()
	{
		ApacheHttpClientConfig config = new DefaultApacheHttpClientConfig();
		config.getProperties().put(ApacheHttpClientConfig.PROPERTY_HANDLE_COOKIES, true);
		return ApacheHttpClient.create(config);
	}

	// @Override
	// public WebTarget target()
	// {
	// if (trg == null)
	// {
	// ClientConfig config = new ClientConfig();
	// JerseyClient client = JerseyClientBuilder.createClient(config);
	// trg = client.target(getBaseURI());
	// }
	// return trg;
	// }

	@Test
	public void unauthTest()
	{
		logoutTest();

		final String url = BASE_URL + "/api/diary/view";
		String str = getClient().resource(url).accept(MediaType.APPLICATION_JSON).get(String.class);

		System.out.println("unauthTest(): " + str);

		// assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_UNAUTHORIZED, r.getCode());
	}

	@Test
	public void logoutTest()
	{
		final String url = BASE_URL + "/api/auth/logout";
		String str = getClient().resource(url).accept(MediaType.APPLICATION_JSON).get(String.class);
		System.out.println("logoutTest(): " + str);

		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	}

	@Test
	public void loginTest_correct()
	{
		WebResource request = getClient().resource(BASE_URL + "/api/auth/login");
		request = request.queryParam("login", demoLogin);
		request = request.queryParam("pass", demoPass);
		request = request.queryParam("api", "20");
		String str = request.accept(MediaType.APPLICATION_JSON).post(String.class);

		System.out.println("loginTest(): " + str);

		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	}

	@Test
	public void loginTest_wrong()
	{
		WebResource request = getClient().resource(BASE_URL + "/api/auth/login");
		request = request.queryParam("login", demoLogin);
		request = request.queryParam("pass", "wrong");
		request = request.queryParam("api", "20");
		String str = request.accept(MediaType.APPLICATION_JSON).post(String.class);

		System.out.println("loginTest(): " + str);

		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_BADCREDENTIALS, r.getCode());
	}

	@Test
	public void loginGETTest()
	{
		WebResource request = getClient().resource(BASE_URL + "/api/auth/login");
		request = request.queryParam("login", demoLogin);
		request = request.queryParam("pass", demoPass);
		request = request.queryParam("api", "20");
		String str = request.accept(MediaType.APPLICATION_JSON).post(String.class);

		System.out.println("loginGETTest(): " + str);
		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	}

	@Test
	public void doubleCheck()
	{
		WebResource request = getClient().resource(BASE_URL + "/api/auth/login");
		request = request.queryParam("login", demoLogin);
		request = request.queryParam("pass", demoPass);
		request = request.queryParam("api", "20");
		String str = request.accept(MediaType.APPLICATION_JSON).post(String.class);

		System.out.println("doubleCheck: resp[1] = " + str);

		// =============================================================

		request = getClient().resource(BASE_URL + "/api/diary/new");
		request = request.queryParam("mod_after", "2012-01-01%2012:15:42");
		str = request.accept(MediaType.APPLICATION_JSON).get(String.class);

		System.out.println("doubleCheck: resp[2] = " + str);

	}

	// @Test
	// public void loginTest2()
	// {
	// String url = "/api/auth/login";
	// Builder request = target().path(url).queryParam("login", demoLogin).queryParam("pass",
	// demoPass)
	// .queryParam("api", API_VERSION).request(MediaType.APPLICATION_JSON);
	// String response = request.post(null, String.class);
	//
	// System.out.println("loginTest2() [auth]: " + response);
	//
	// StdResponse r = StdResponse.decode(response.toString());
	// assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	//
	// // ==================================================================
	//
	// url = "/api/auth/check";
	// request = target().path(url).request(MediaType.APPLICATION_JSON);
	// Response resp = request.get();
	// String text = resp.readEntity(String.class);
	//
	// System.out.println("loginTest2() [check]: " + text);
	// assertEquals("Logged", text);
	//
	// // ==================================================================
	//
	// url = "/api/diary/new";
	// request = target().path(url).queryParam("mod_after",
	// "2012-01-01 12:00:00").request(MediaType.APPLICATION_JSON);
	// resp = request.get();
	//
	// System.out.println("loginTest2() [diary]: " + resp.readEntity(String.class));
	// }

	// @Test
	// public void loginTest3()
	// {
	// final String base = "http://localhost:8082/CompServer/api/";
	// Client client = ClientBuilder.newClient();
	//
	// WebTarget webTarget = client.target(base + "auth/set");
	// webTarget = webTarget.queryParam("value", "42");
	// String response = webTarget.request(MediaType.APPLICATION_JSON).get(String.class);
	// System.out.println("loginTest3() [auth:set]: " + response);
	//
	// // ==================================================================
	//
	// webTarget = client.target(base + "auth/set");
	// webTarget = webTarget.queryParam("value", "43");
	// response = webTarget.request(MediaType.APPLICATION_JSON).get(String.class);
	// System.out.println("loginTest3() [auth:set]: " + response);
	//
	// // ==================================================================
	//
	// webTarget = client.target(base + "auth/get");
	// response = webTarget.request(MediaType.APPLICATION_JSON).get(String.class);
	// System.out.println("loginTest3() [auth:get]: " + response);
	//
	// // ==================================================================
	//
	// webTarget = client.target(base + "diary/get");
	// response = webTarget.request(MediaType.APPLICATION_JSON).get(String.class);
	// System.out.println("loginTest3() [diary:get]: " + response);
	//
	// // url = "/api/diary/get";
	// // request = target().path(url).request(MediaType.APPLICATION_JSON);
	// // response = request.get(String.class);
	// // System.out.println("loginTest3() [diary:get]: " + response);
	// }
}
