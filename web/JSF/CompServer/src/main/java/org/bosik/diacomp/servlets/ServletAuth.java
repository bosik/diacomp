package org.bosik.diacomp.servlets;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.bosik.diacomp.utils.ResponseBuilder;

@WebServlet
public class ServletAuth extends HttpServlet
{
	private static final long	serialVersionUID	= 1L;
	private static final String	PAR_USERID			= "USER_ID";
	private static final int	INVALID_USER		= -1;

	public ServletAuth()
	{

	}

	public static boolean checkAuth(HttpServletRequest request)
	{
		return request.getSession().getAttribute(PAR_USERID) != null;
	}

	public static int getCurrentUserId(HttpServletRequest request)
	{
		return (Integer) request.getSession().getAttribute(PAR_USERID);
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		// FIXME: make it POST

		response.setContentType("text/html");
		final PrintWriter out = response.getWriter();

		if ((request.getParameter("login") != null) && (request.getParameter("pass") != null))
		{
			String login = request.getParameter("login");
			String pass = request.getParameter("pass");
			int id = authentificate(login, pass);

			if (id != INVALID_USER)
			{
				request.getSession().setAttribute(PAR_USERID, id);
				response.setStatus(HttpServletResponse.SC_OK);
				out.print(ResponseBuilder.buildDone("Logged in OK"));
			}
			else
			{
				response.setStatus(HttpServletResponse.SC_OK);
				out.print(ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS, "Bad username/password"));
			}
		}
		else if (request.getParameter("logout") != null)
		{
			request.getSession().setAttribute(PAR_USERID, null);

			response.setStatus(HttpServletResponse.SC_OK);
			out.print(ResponseBuilder.buildDone("Logged out OK"));
		}
		else
		{
			response.setStatus(HttpServletResponse.SC_NOT_FOUND);
		}
	}

	private static int authentificate(String login, String pass)
	{
		if ("admin".equals(login) && "1234".equals(pass))
		{
			return 42;
		}

		return INVALID_USER;
	}
}
