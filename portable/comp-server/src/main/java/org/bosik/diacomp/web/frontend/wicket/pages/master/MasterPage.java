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
package org.bosik.diacomp.web.frontend.wicket.pages.master;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.diacomp.web.frontend.wicket.components.header.HeaderPanel;
import org.bosik.diacomp.web.frontend.wicket.components.menu.Menu;
import org.bosik.diacomp.web.frontend.wicket.components.menu.MenuContent;
import org.bosik.diacomp.web.frontend.wicket.components.menu.MenuItem;
import org.bosik.diacomp.web.frontend.wicket.pages.about.AboutPage;
import org.bosik.diacomp.web.frontend.wicket.pages.base.FoodBasePage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;
import org.bosik.diacomp.web.frontend.wicket.pages.download.DownloadPage;
import org.bosik.diacomp.web.frontend.wicket.pages.login.LoginPage;
import org.bosik.diacomp.web.frontend.wicket.pages.stats.StatsPage;

public class MasterPage extends WebPage
{
	private static final long	serialVersionUID	= 1L;

	@SpringBean
	private UserInfoService		userInfoService;

	public MasterPage(final PageParameters parameters)
	{
		super(parameters);

		try
		{
			String userName = userInfoService.getCurrentUserName();
			add(new HeaderPanel("headerPanel", userName));
			add(new Menu("menu", Model.of(getMenu(true))));
		}
		catch (NotAuthorizedException e)
		{
			add(new HeaderPanel("headerPanel"));
			add(new Menu("menu", Model.of(getMenu(false))));
		}

		add(new Label("pageTitle", getString("res.appTitle")));
		add(new Label("textVersion", Config.get("DIACOMP_VERSION")));
	}

	protected MenuContent getMenu(boolean authorized)
	{
		MenuContent menuContent = new MenuContent();

		if (authorized)
		{
			menuContent.getItems().add(new MenuItem(getString("menu.about"), AboutPage.class));
			menuContent.getItems().add(new MenuItem(getString("menu.diary"), DiaryPage.class));
			menuContent.getItems().add(new MenuItem(getString("menu.base"), FoodBasePage.class));
			menuContent.getItems().add(new MenuItem(getString("menu.stats"), StatsPage.class));
			menuContent.getItems().add(new MenuItem(getString("menu.download"), DownloadPage.class));
		}
		else
		{
			menuContent.getItems().add(new MenuItem(getString("menu.about"), AboutPage.class));
			menuContent.getItems().add(new MenuItem(getString("menu.login"), LoginPage.class));
		}

		menuContent.setSelected(getClass());

		return menuContent;
	}
}
