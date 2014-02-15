package org.bosik.diacomp.features.auth.dao;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import org.bosik.diacomp.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.services.exceptions.UnsupportedAPIException;

public class FakeAuthDAO implements AuthDAO
{
	private static final String	PAR_USERID		= "USER_ID";
	private static final int	INVALID_USER	= -1;

	private static final int	API_CURRENT		= 20;
	private static final int	API_LEGACY		= 19;

	@Override
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

	@Override
	public int getCurrentUserId(HttpServletRequest request)
	{
		return (Integer) request.getSession().getAttribute(PAR_USERID);
	}

	@Override
	public void login(HttpServletRequest request, String login, String pass, int apiVersion)
	{
		final HttpSession session = request.getSession();

		if (apiVersion < API_LEGACY)
		{
			String msg = String.format("API %d is unsupported. The latest API: %d. Legacy API: %d.", apiVersion,
					API_CURRENT, API_LEGACY);
			throw new UnsupportedAPIException(msg);
		}

		if (apiVersion < API_CURRENT)
		{
			String msg = String.format(
					"API %d is still supported, but deprecated. The latest API: %d. Legacy API: %d.", apiVersion,
					API_CURRENT, API_LEGACY);
			throw new DeprecatedAPIException(msg);
		}

		if ("admin".equals(login) && "1234".equals(pass))
		{
			int id = 1;
			session.setAttribute(PAR_USERID, id);
		}
		else
		{
			session.removeAttribute(PAR_USERID);
			throw new NotAuthorizedException();
		}
	}

	@Override
	public void logout(HttpServletRequest request)
	{
		request.getSession().removeAttribute(PAR_USERID);
	}
}
