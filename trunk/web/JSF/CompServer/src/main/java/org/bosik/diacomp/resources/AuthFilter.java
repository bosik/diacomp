package org.bosik.diacomp.resources;

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
import org.bosik.diacomp.utils.ResponseBuilder;

@WebFilter("/AuthenticationFilter")
public class AuthFilter implements Filter
{
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
		HttpServletRequest req = (HttpServletRequest) request;
		HttpServletResponse res = (HttpServletResponse) response;

		System.out.println(req.getServletPath());

		if (req.getServletPath().startsWith("/api/login") || AuthResource.checkAuth(req) || true)
		{
			System.out.println("Requested OK...");
			chain.doFilter(request, response);
		}
		else
		{
			System.err.println("Not authorized request!");
			// res.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Who r u?");
			res.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
			res.getWriter().write(ResponseBuilder.build(ResponseBuilder.CODE_UNAUTHORIZED, "Not authorized"));
		}
	}

	@Override
	public void init(FilterConfig fConfig) throws ServletException
	{

	}
}
