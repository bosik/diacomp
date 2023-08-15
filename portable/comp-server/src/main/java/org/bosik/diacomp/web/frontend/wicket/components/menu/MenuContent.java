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

import org.apache.wicket.markup.html.WebPage;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class MenuContent implements Serializable
{
	private static final long			serialVersionUID	= 1L;

	private final List<MenuItem>		items				= new ArrayList<MenuItem>();
	private Class<? extends WebPage>	selected;
	private String						userName;

	public List<MenuItem> getItems()
	{
		return items;
	}

	public Class<? extends WebPage> getSelected()
	{
		return selected;
	}

	public void setSelected(Class<? extends WebPage> selected)
	{
		this.selected = selected;
	}

	public String getUserName()
	{
		return userName;
	}

	public void setUserName(String userName)
	{
		this.userName = userName;
	}
}
