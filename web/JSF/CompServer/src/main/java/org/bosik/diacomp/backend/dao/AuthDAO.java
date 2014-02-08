package org.bosik.diacomp.backend.dao;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.Context;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;

// TODO: extract interface
public class AuthDAO
{
	@Context
	HttpServletRequest			req;

	private static final String	PAR_USERID		= "USER_ID";
	private static final int	INVALID_USER	= -1;

	// TODO: implement logout

	// TODO: seems bad approach
	public void checkAuth(HttpServletRequest request) throws NotAuthorizedException
	{
		// FIXME: test if this works, remove HttpServletRequest if yes
		// request = req;

		if ((request != null) && (request.getSession(false) != null)
				&& (request.getSession().getAttribute(PAR_USERID) != null)
				&& ((Integer) request.getSession().getAttribute(PAR_USERID) > INVALID_USER))
		{
			// it's ok
		}
		else
		{
			throw new NotAuthorizedException();
		}
	}

	public int getCurrentUserId(HttpServletRequest request)
	{
		// request = req;

		return (Integer) request.getSession().getAttribute(PAR_USERID);
	}

	public void login(HttpServletRequest request, String login, String pass)
	{
		// request = req;

		if ("admin".equals(login) && "1234".equals(pass))
		{
			int id = 1;
			request.getSession().setAttribute(PAR_USERID, id);
		}
		else
		{
			request.getSession().setAttribute(PAR_USERID, INVALID_USER);
			throw new NotAuthorizedException();
		}
	}

	public void logout(HttpServletRequest request)
	{
		// TODO: implement
	}
}
