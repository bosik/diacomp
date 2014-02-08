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

	/**
	 * @see Filter#doFilter(ServletRequest, ServletResponse, FilterChain)
	 */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException,
			ServletException
	{
		final String BASE_URL = "/CompServer";
		HttpServletRequest req = (HttpServletRequest) request;
		HttpServletResponse res = (HttpServletResponse) response;

		// System.out.println("doFilter(): getServletPath=" + req.getServletPath());
		// System.out.println("doFilter(): getRequestURL=" + req.getRequestURL());
		// System.out.println("doFilter(): getRequestURI=" + req.getRequestURI());
		// System.out.println("doFilter(): getQueryString=" + req.getQueryString());

		if (req.getRequestURI().startsWith(BASE_URL + "/api/auth/"))
		{
			System.out.println("Requested OK...");
			chain.doFilter(request, response);
		}
		else if (req.getRequestURI().startsWith(BASE_URL + "/api/info"))
		{
			System.out.println("Requested OK...");
			chain.doFilter(request, response);
		}
		else
		{
			try
			{
				authService.checkAuth(req);
				System.out.println("Requested OK...");
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
