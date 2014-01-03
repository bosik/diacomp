package org.bosik.diacomp.servlets;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@WebServlet
public class ServletDiary extends HttpServlet
{
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{

		// int a = Integer.parseInt(request.getParameter("a"));
		// int b = Integer.parseInt(request.getParameter("b"));
		// out.print("<h1>Hello, " + id + "! a + b = " + (a + b) + "</h1>");

		// response.getWriter().println("session=" + request.getSession(true).getId());

	}

	@Override
	protected void doPut(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException
	{
		// TODO Auto-generated method stub
		super.doPut(req, resp);
	}
}
