package org.bosik.diacomp.backend.filters;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.bosik.diacomp.backend.dao.AuthDAO;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.utils.ResponseBuilder;

@WebFilter("/AuthenticationFilter")
public class AuthFilter implements Filter
{
	private AuthDAO	authService	= new AuthDAO();

	public AuthFilter()
	{

	}

	@Override
	public void destroy()
	{

	}

	// private static void printRequestInfo(HttpServletRequest request)
	// {
	// System.out.println("Request info:");
	//
	// if (request.getSession(false) == null)
	// {
	// System.out.println("\tSession is null");
	// }
	// else
	// {
	// System.out.println("\tSession is setted");
	// final HttpSession session = request.getSession();
	//
	// final Enumeration<String> attributeNames = session.getAttributeNames();
	// while (attributeNames.hasMoreElements())
	// {
	// String attr = attributeNames.nextElement();
	// Object val = session.getAttribute(attr);
	// System.out.println("\t" + attr + "=" + val);
	// }
	// }
	// }

	/**
	 * @see Filter#doFilter(ServletRequest, ServletResponse, FilterChain)
	 */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException,
			ServletException
	{
		HttpServletRequest req = (HttpServletRequest)request;
		HttpServletResponse res = (HttpServletResponse)response;

		final String BASE_URL = req.getContextPath();

		System.out.println("doFilter(): getContextPath=" + req.getContextPath());
		System.out.println("doFilter(): getServletPath=" + req.getServletPath());
		System.out.println("doFilter(): getRequestURL=" + req.getRequestURL());
		System.out.println("Requested: " + req.getRequestURI());
		System.out.println("doFilter(): getQueryString=" + req.getQueryString());

		if (req.getRequestURI().startsWith(BASE_URL + "/api/auth/"))
		{
			System.out.println("Mode: public");
			chain.doFilter(request, response);
		}
		else if (req.getRequestURI().startsWith(BASE_URL + "/api/info/"))
		{
			System.out.println("Mode: public");
			chain.doFilter(request, response);
		}
		else
		{
			System.out.println("Mode: private");
			//printRequestInfo(req);
			try
			{
				authService.checkAuth(req);
				System.out.println("Authentified OK, chained");
				chain.doFilter(request, response);
			}
			catch (NotAuthorizedException e)
			{
				System.err.println("Not authorized request!");
				// res.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
				res.getWriter().write(ResponseBuilder.build(ResponseBuilder.CODE_UNAUTHORIZED, "Not authorized"));
			}
		}
	}

	@Override
	public void init(FilterConfig fConfig) throws ServletException
	{

	}
}
