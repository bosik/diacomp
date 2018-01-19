/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.common;

import javax.servlet.http.HttpServletRequest;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;

public class UserSessionUtils
{
	private static final String	PAR_USERID		= "USER_ID";
	private static final String	PAR_USERNAME	= "USER_NAME";

	public static void setId(HttpServletRequest request, int id, String userName)
	{
		request.getSession().setAttribute(PAR_USERID, id);
		request.getSession().setAttribute(PAR_USERNAME, userName);
	}

	public static int getId(HttpServletRequest request)
	{
		checkAuth(request);
		return (Integer)request.getSession().getAttribute(PAR_USERID);
	}

	public static void clearId(HttpServletRequest request)
	{
		request.getSession().removeAttribute(PAR_USERID);
	}

	public static void checkAuth(HttpServletRequest request) throws NotAuthorizedException
	{
		if ((request == null) || (request.getSession(false) == null)
				|| (request.getSession().getAttribute(PAR_USERID) == null)
				|| ((Integer)request.getSession().getAttribute(PAR_USERID) <= 0))
		{
			throw new NotAuthorizedException();
		}
	}

	public static String getUserName(HttpServletRequest request)
	{
		checkAuth(request);
		return (String)request.getSession().getAttribute(PAR_USERNAME);
	}
}
