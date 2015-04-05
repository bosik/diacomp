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
package org.bosik.diacomp.web.frontend.wicket.pages.clientinfo;

import javax.servlet.http.HttpServletResponse;
import org.apache.wicket.markup.html.pages.BrowserInfoPage;
import org.apache.wicket.request.flow.RedirectToUrlException;

public class CustomBrowserInfoPage extends BrowserInfoPage
{
	private static final long	serialVersionUID	= 1L;

	/**
	 * Called from browser when URL typed directly; redirects to home page
	 */
	public CustomBrowserInfoPage()
	{
		CharSequence url = urlFor(getApplication().getHomePage(), null);
		throw new RedirectToUrlException(url.toString(), HttpServletResponse.SC_MOVED_TEMPORARILY);
	}

	/**
	 * For calling internally; argument added just to distinguish this constructor from default one
	 * 
	 * @param x
	 *            Pass anything you want
	 */
	public CustomBrowserInfoPage(int x)
	{
		super();
	}
}
