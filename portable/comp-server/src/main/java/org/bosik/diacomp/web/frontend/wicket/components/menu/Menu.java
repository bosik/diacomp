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

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.web.frontend.wicket.pages.about.AboutPage;
import org.bosik.diacomp.web.frontend.wicket.pages.settings.SettingsPage;

public class Menu extends GenericPanel<MenuContent>
{
	private static final long serialVersionUID = 1L;

	public Menu(String id, IModel<MenuContent> content)
	{
		super(id);
		setModel(content);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		add(new BookmarkablePageLink<Void>("linkAbout", AboutPage.class));

		RepeatingView menu = new RepeatingView("menuItem");
		add(menu);

		for (final MenuItem menuItem : getModelObject().getItems())
		{
			BookmarkablePageLink<Void> link = new BookmarkablePageLink<Void>("Link", menuItem.getResponsePage());
			link.add(new Label("Text", menuItem.getCaption()));
			WebMarkupContainer itemPlace = new WebMarkupContainer(menu.newChildId());
			menu.add(itemPlace);
			itemPlace.add(link);

			final String current = menuItem.getResponsePage().getName();
			final String selected = getModelObject().getSelected().getName();
			if (current.equals(selected))
			{
				itemPlace.add(new AttributeModifier("class", "active"));
			}
			else
			{
				//link.add(new AttributeAppender("class", " menu_option"));
			}
		}

		String userName = getModelObject().getUserName();
		MarkupContainer loginInfo = new WebMarkupContainer("infoLogin");
		add(loginInfo.setVisible(!"".equals(userName))); // TODO
		loginInfo.add(new ExternalLink("linkLogout", "/logout"));
		loginInfo.add(new BookmarkablePageLink("linkSettings", SettingsPage.class).add(new Label("userName", userName)));
	}
}
