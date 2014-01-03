package org.bosik.diacomp;

import static org.junit.Assert.fail;
import java.net.URI;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation.Builder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.UriBuilder;
import org.bosik.diacomp.resources.MyApp;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.JerseyClient;
import org.glassfish.jersey.client.JerseyClientBuilder;
import org.glassfish.jersey.internal.util.collection.MultivaluedStringMap;
import org.glassfish.jersey.test.JerseyTest;
import org.junit.Test;

public class TestAuth extends JerseyTest
{
	public TestAuth() throws Exception
	{
		super(MyApp.class);
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
	public void simpleGetTest()
	{
		final String url = "/api/auth/login";
		final String login = "admin";
		final String pass = "1234";
		String resp = target().path(url).queryParam("login", login).queryParam("pass", pass)
				.request(MediaType.APPLICATION_JSON).get(String.class);

		System.out.println(resp);
	}

	@Test
	public void test()
	{
		MultivaluedMap<String, String> formData = new MultivaluedStringMap();
		formData.add("login", "admin");
		formData.add("pass", "1234");

		final Builder request = target().path("/api/auth/login").request(MediaType.APPLICATION_JSON);
		final Entity<MultivaluedMap<String, String>> entity = Entity.entity(formData, MediaType.APPLICATION_JSON);
		String response = request.post(entity, String.class);

		System.err.println(response);

		// ============================================
		//
		// Builder builder = service.request(MediaType.APPLICATION_JSON);
		// builder.accept(MediaType.APPLICATION_JSON);
		// builder.post(Example.class, example);
		//
		// try{
		// response = service.request(MediaType.APPLICATION_XML).post(ClientResponse.class,
		// request);
		//
		// service.request(MediaType.APPLICATION_JSON).post(searchQuery,
		// MediaType.APPLICATION_JSON);
		// ListWrapper listWrapper = response.getEntity(ListWrapper.class);
		//
		// System.out.println(response.getStatus());
		// if(response.getStatus() == 200){
		// EmpResponse empResponse = response.getEntity(EmpResponse.class);
		// System.out.println(empResponse.getId() + "::"+empResponse.getName());
		// }else{
		// ErrorResponse exc = response.getEntity(ErrorResponse.class);
		// System.out.println(exc.getErrorCode());
		// System.out.println(exc.getErrorId());
		// }
		// }catch(Exception e){
		// System.out.println(e.getMessage());
		// }
	}

	// private static URI getBaseURI()
	// {
	// return UriBuilder.fromUri("http://localhost:8082/CompServer").build();
	// }
}
