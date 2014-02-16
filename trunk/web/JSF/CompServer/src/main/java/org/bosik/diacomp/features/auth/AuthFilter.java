package org.bosik.diacomp.features.auth;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.bosik.diacomp.features.auth.dao.AuthDAO;
import org.bosik.diacomp.features.auth.dao.FakeAuthDAO;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.utils.ResponseBuilder;

@WebFilter("/AuthenticationFilter")
public class AuthFilter implements Filter
{
	private AuthDAO						authService	= new FakeAuthDAO();

	private static final List<String>	PUBLIC_URLS	= new LinkedList<String>();
	{
		PUBLIC_URLS.clear();
		PUBLIC_URLS.add("/api/auth/");
		PUBLIC_URLS.add("/api/info/");
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
		HttpServletRequest req = (HttpServletRequest) request;
		HttpServletResponse res = (HttpServletResponse) response;

		// System.out.println("doFilter(): getContextPath=" + req.getContextPath());
		// System.out.println("doFilter(): getServletPath=" + req.getServletPath());
		// System.out.println("doFilter(): getRequestURL=" + req.getRequestURL());
		// System.out.println("Requested: " + req.getRequestURI());
		// System.out.println("doFilter(): getQueryString=" + req.getQueryString());

		final String BASE_URL = req.getContextPath();
		final String requestURI = req.getRequestURI();

		for (String url : PUBLIC_URLS)
		{
			if (requestURI.startsWith(BASE_URL + url))
			{
				// System.out.println("Mode: public");
				chain.doFilter(request, response);
				return;
			}
		}

		// System.out.println("Mode: private");
		// printRequestInfo(req);
		try
		{
			UserService.checkAuth(req);
			// System.out.println("Authentified OK, chained");
			chain.doFilter(request, response);
		}
		catch (NotAuthorizedException e)
		{
			System.err.println("Not authorized request!");
			// res.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
			res.getWriter().write(ResponseBuilder.build(ResponseBuilder.CODE_UNAUTHORIZED, "Not authorized"));
		}
	}

	@Override
	public void init(FilterConfig fConfig) throws ServletException
	{

	}
}
