package org.bosik.diacomp.servlets;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;

// TODO: remove

@WebServlet
public class ServletAuth extends HttpServlet
{
	// private static final long serialVersionUID = 1L;
	//
	// public ServletAuth()
	// {
	//
	// }
	//
	// @Override
	// protected void doGet(HttpServletRequest request, HttpServletResponse response) throws
	// ServletException, IOException
	// {
	// // FIXME: make it POST
	//
	// response.setContentType("text/json");
	// final PrintWriter out = response.getWriter();
	//
	// if ((request.getParameter("login") != null) && (request.getParameter("pass") != null))
	// {
	// String login = request.getParameter("login");
	// String pass = request.getParameter("pass");
	// int id = authentificate(login, pass);
	//
	// if (id != INVALID_USER)
	// {
	// request.getSession().setAttribute(PAR_USERID, id);
	// response.setStatus(HttpServletResponse.SC_OK);
	// out.print(ResponseBuilder.buildDone("Logged in OK"));
	// }
	// else
	// {
	// response.setStatus(HttpServletResponse.SC_OK);
	// out.print(ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS,
	// "Bad username/password"));
	// }
	// }
	// else if (request.getParameter("logout") != null)
	// {
	// request.getSession().setAttribute(PAR_USERID, null);
	//
	// response.setStatus(HttpServletResponse.SC_OK);
	// out.print(ResponseBuilder.buildDone("Logged out OK"));
	// }
	// else
	// {
	// response.setStatus(HttpServletResponse.SC_NOT_FOUND);
	// }
	// }

}
