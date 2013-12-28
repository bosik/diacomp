package org.bosik.diacomp.servlets;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@WebServlet
public class ServletAuth extends HttpServlet
{
	private static final long	serialVersionUID	= 1L;
	private static final String	PAR_USERID			= "USER_ID";

	public ServletAuth()
	{

	}

	public static boolean checkAuth(HttpServletRequest request)
	{
		return request.getSession().getAttribute(PAR_USERID) != null;
	}

	public static int getCurrentUserId(HttpServletRequest request)
	{
		return Integer.parseInt((String) request.getSession().getAttribute(PAR_USERID));
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		// FIXME: make it POST

		response.setContentType("text/html");
		final PrintWriter out = response.getWriter();

		String login = request.getParameter("login");
		String pass = request.getParameter("pass");

		if ("admin".equals(login) && "1234".equals(pass))
		{
			request.getSession().setAttribute(PAR_USERID, "42");

			response.setStatus(HttpServletResponse.SC_OK);
			out.println("{code:0, response:\"Logged in OK\"}");
		}
		else if (request.getParameter("logout") != null)
		{
			request.getSession().setAttribute(PAR_USERID, null);

			response.setStatus(HttpServletResponse.SC_OK);
			out.println("{code:0, response:\"Logged out OK\"}");
		}
		else
		{
			response.setStatus(HttpServletResponse.SC_OK);
			out.println("{code:1, response:\"Bad username/password\"}");
		}
	}
}
