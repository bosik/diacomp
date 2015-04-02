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
package org.bosik.diacomp.web.frontend.wicket.components.header;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.bosik.diacomp.web.frontend.wicket.pages.about.AboutPage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;

public class HeaderPanel extends Panel
{
	private static final long	serialVersionUID	= 1L;

	/**
	 * Header for guest
	 * 
	 * @param id
	 */
	public HeaderPanel(String id)
	{
		super(id);

		add(new ExternalLink("linkLogout", "j_spring_security_logout").setVisible(false));
		add(new BookmarkablePageLink<Void>("linkHome", AboutPage.class));
		add(new Label("infoLogin"));
	}

	/**
	 * Header for authorized user
	 * 
	 * @param id
	 * @param userName
	 */
	public HeaderPanel(String id, String userName)
	{
		super(id);

		add(new ExternalLink("linkLogout", "j_spring_security_logout"));
		add(new BookmarkablePageLink<Void>("linkHome", DiaryPage.class));
		add(new Label("infoLogin", userName));
	}
}
