package org.bosik.diacomp.web.backend.features.auth.rest;

@Deprecated
public class AuthRestClient //extends RestClient implements AuthService
{
	//	private static final long	serialVersionUID	= 1L;
	//
	//	@Override
	//	public void login(String login, String pass, int apiVersion)
	//	{
	//		WebResource resource = getResource("api/auth/login");
	//		try
	//		{
	//			Form form = new Form();
	//			form.add("login", login);
	//			form.add("pass", pass);
	//			form.add("api", String.valueOf(apiVersion));
	//			String str = resource.accept(MediaType.APPLICATION_JSON).post(String.class, form);
	//
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			throw new CommonServiceException("URL: " + resource.getURI(), e);
	//		}
	//	}
	//
	//	@Override
	//	public void logout()
	//	{
	//		WebResource resource = getResource("api/auth/logout");
	//		try
	//		{
	//			String str = resource.accept(MediaType.APPLICATION_JSON).get(String.class);
	//
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			throw new CommonServiceException("URL: " + resource.getURI(), e);
	//		}
	//	}
}
