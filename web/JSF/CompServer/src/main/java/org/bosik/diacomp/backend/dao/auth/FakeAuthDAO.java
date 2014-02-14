package org.bosik.diacomp.backend.dao.auth;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.bosik.diacomp.services.exceptions.NotAuthorizedException;

// TODO: extract interface
public class FakeAuthDAO
{
	private static final String	PAR_USERID		= "USER_ID";
	private static final int	INVALID_USER	= -1;

	// TODO: seems bad approach
	public void checkAuth(HttpServletRequest request) throws NotAuthorizedException
	{
		// ================================================================
		// final Enumeration<String> attributeNames = request.getSession().getAttributeNames();
		// System.out.println("checkAuth, attributes are:");
		// while (attributeNames.hasMoreElements())
		// {
		// String attr = attributeNames.nextElement();
		// Object val = request.getSession().getAttribute(attr);
		// System.out.println("\t" + attr + "=" + val);
		// }
		// ================================================================

		if ((request == null) || (request.getSession(false) == null)
				|| (request.getSession().getAttribute(PAR_USERID) == null)
				|| ((Integer) request.getSession().getAttribute(PAR_USERID) <= INVALID_USER))
		{
			throw new NotAuthorizedException();
		}
	}

	public int getCurrentUserId(HttpServletRequest request)
	{
		return (Integer) request.getSession().getAttribute(PAR_USERID);
	}

	public void login(HttpServletRequest request, String login, String pass)
	{
		final HttpSession session = request.getSession();

		if ("admin".equals(login) && "1234".equals(pass))
		{
			int id = 1;
			session.setAttribute(PAR_USERID, id);
		}
		else
		{
			// request.getSession(true).setAttribute(PAR_USERID, INVALID_USER);
			session.removeAttribute(PAR_USERID);

			throw new NotAuthorizedException();
		}
	}

	public void logout(HttpServletRequest request)
	{
		request.getSession().removeAttribute(PAR_USERID);
	}
}
