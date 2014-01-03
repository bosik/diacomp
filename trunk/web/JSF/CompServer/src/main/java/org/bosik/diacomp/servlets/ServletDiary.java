package org.bosik.diacomp.servlets;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.bosik.diacomp.MySQLAccess;
import org.bosik.diacomp.resources.AuthResource;

@WebServlet
public class ServletDiary extends HttpServlet
{
	private static final long	serialVersionUID	= 1L;

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

		int id = AuthResource.getCurrentUserId(request);

		// int a = Integer.parseInt(request.getParameter("a"));
		// int b = Integer.parseInt(request.getParameter("b"));
		// out.print("<h1>Hello, " + id + "! a + b = " + (a + b) + "</h1>");

		MySQLAccess dao = new MySQLAccess();
		dao.readDataBase();

		// response.getWriter().println("session=" + request.getSession(true).getId());

	}

	@Override
	protected void doPut(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException
	{
		// TODO Auto-generated method stub
		super.doPut(req, resp);
	}
}
