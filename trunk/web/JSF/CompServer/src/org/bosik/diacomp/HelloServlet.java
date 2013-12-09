package org.bosik.diacomp;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class HelloServlet
 */
@WebServlet(description = "Some description goes here", urlPatterns = { "/HelloServlet" })
public class HelloServlet extends HttpServlet
{
	private static final long	serialVersionUID	= 1L;

	/**
	 * Default constructor.
	 */
	public HelloServlet()
	{
		// TODO Auto-generated constructor stub
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		response.setContentType("text/html");
		response.setStatus(HttpServletResponse.SC_OK);

		String a = request.getParameter("a");
		String b = request.getParameter("b");

		response.getWriter().println("<h1>Hello Servlet</h1>");
		response.getWriter().println("a = " + a + "<br/>");
		response.getWriter().println("b = " + b + "<br/>");
		response.getWriter().println("a+b = " + a + b + "<br/>");
		response.getWriter().println("session=" + request.getSession(true).getId());
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException
	{
		// TODO Auto-generated method stub
	}
}
