package org.bosik.diacomp.web.backend.features.auth;

import static org.junit.Assert.assertEquals;
import javax.ws.rs.core.MediaType;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.web.backend.common.Config;
import org.junit.Test;
import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.client.apache.ApacheHttpClient;
import com.sun.jersey.client.apache.config.ApacheHttpClientConfig;
import com.sun.jersey.client.apache.config.DefaultApacheHttpClientConfig;

public class TestAuthResource
{
	private static Client getClient()
	{
		ApacheHttpClientConfig config = new DefaultApacheHttpClientConfig();
		config.getProperties().put(ApacheHttpClientConfig.PROPERTY_HANDLE_COOKIES, true);
		return ApacheHttpClient.create(config);
	}

	protected static WebResource getResource(String url)
	{
		return getClient().resource(Config.getBaseURL() + url);
	}

	@Test
	public void unauthTest()
	{
		logoutTest();

		String str = getResource("api/diary/view").accept(MediaType.APPLICATION_JSON).get(String.class);

		// System.out.println("unauthTest(): " + str);

		// assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_UNAUTHORIZED, r.getCode());
	}

	@Test
	public void logoutTest()
	{
		String str = getResource("api/auth/logout").accept(MediaType.APPLICATION_JSON).get(String.class);

		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	}

	@Test
	public void loginTest_correct()
	{
		WebResource request = getResource("api/auth/login");
		request = request.queryParam("login", Config.getLogin());
		request = request.queryParam("pass", Config.getPassword());
		request = request.queryParam("api", String.valueOf(Config.getAPICurrent()));
		String str = request.accept(MediaType.APPLICATION_JSON).post(String.class);

		// System.out.println("loginTest(): " + str);

		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	}

	@Test
	public void loginTest_wrong()
	{
		WebResource request = getResource("api/auth/login");
		request = request.queryParam("login", Config.getLogin());
		request = request.queryParam("pass", "wrong");
		request = request.queryParam("api", String.valueOf(Config.getAPICurrent()));
		String str = request.accept(MediaType.APPLICATION_JSON).post(String.class);

		// System.out.println("loginTest(): " + str);

		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_BADCREDENTIALS, r.getCode());
	}

	@Test
	public void loginGETTest()
	{
		WebResource request = getResource("api/auth/login");
		request = request.queryParam("login", Config.getLogin());
		request = request.queryParam("pass", Config.getPassword());
		request = request.queryParam("api", String.valueOf(Config.getAPICurrent()));
		String str = request.accept(MediaType.APPLICATION_JSON).post(String.class);

		// System.out.println("loginGETTest(): " + str);
		StdResponse r = StdResponse.decode(str);
		assertEquals(ResponseBuilder.CODE_OK, r.getCode());
	}

	// TODO: remove all REST tests; test functions (BE or FE) only

	@Test
	public void doubleCheck()
	{
		WebResource request = getResource("api/auth/login");
		request = request.queryParam("login", Config.getLogin());
		request = request.queryParam("pass", Config.getPassword());
		request = request.queryParam("api", String.valueOf(Config.getAPICurrent()));
		String str = request.accept(MediaType.APPLICATION_JSON).post(String.class);

		// System.out.println("doubleCheck: resp[1] = " + str);

		// =============================================================

		request = getResource("/api/diary/new");
		request = request.queryParam("mod_after", "2012-01-01%2012:15:42");
		str = request.accept(MediaType.APPLICATION_JSON).get(String.class);

		// System.out.println("doubleCheck: resp[2] = " + str);

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
