package org.bosik.diacomp.web.backend.common;

import javax.servlet.http.HttpServletRequest;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;

public class UserSessionUtils
{
	private static final String	PAR_USERID	= "USER_ID";

	public static void setId(HttpServletRequest request, int id)
	{
		request.getSession().setAttribute(PAR_USERID, id);
	}

	public static int getId(HttpServletRequest request)
	{
		checkAuth(request);
		return (Integer) request.getSession().getAttribute(PAR_USERID);
	}

	public static void clearId(HttpServletRequest request)
	{
		request.getSession().removeAttribute(PAR_USERID);
	}

	public static void checkAuth(HttpServletRequest request) throws NotAuthorizedException
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
				|| ((Integer) request.getSession().getAttribute(PAR_USERID) <= 0))
		{
			throw new NotAuthorizedException();
		}
	}

}
