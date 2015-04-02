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
package org.bosik.diacomp.web.frontend.wicket.components.menu;

import java.io.Serializable;
import org.apache.wicket.markup.html.WebPage;

public class MenuItem implements Serializable
{
	private static final long			serialVersionUID	= 1L;

	private String						caption;
	private Class<? extends WebPage>	responsePage;

	public MenuItem(String caption, Class<? extends WebPage> responsePage)
	{
		setCaption(caption);
		setResponsePage(responsePage);
	}

	public String getCaption()
	{
		return caption;
	}

	public void setCaption(String caption)
	{
		this.caption = caption;
	}

	public Class<? extends WebPage> getResponsePage()
	{
		return responsePage;
	}

	public void setResponsePage(Class<? extends WebPage> responsePage)
	{
		this.responsePage = responsePage;
	}
}
