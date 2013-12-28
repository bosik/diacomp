package org.bosik.diacomp.servlets;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@WebServlet
public class ServletDiary extends HttpServlet
{
	private static final long	serialVersionUID	= 1L;
	private static final String	PAR_USERID			= "USER_ID";

	public ServletDiary()
	{
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		response.setContentType("text/html");
		final PrintWriter out = response.getWriter();
		response.setStatus(HttpServletResponse.SC_OK);

		if (ServletAuth.checkAuth(request))
		{
			int id = ServletAuth.getCurrentUserId(request);
			out.println("<h1>Hello, " + id + "</h1>");
		}
		else
		{
			out.println("<h1>Not authorized</h1>");
		}

		// response.getWriter().println("session=" + request.getSession(true).getId());

	}

	@Override
	protected void doPut(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException
	{
		// TODO Auto-generated method stub
		super.doPut(req, resp);
	}
}
