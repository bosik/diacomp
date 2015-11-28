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

import java.util.TimeZone;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxClientInfoBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.Model;
import org.apache.wicket.protocol.http.ClientProperties;
import org.apache.wicket.protocol.http.request.WebClientInfo;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.settings.IRequestCycleSettings;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.diacomp.web.frontend.wicket.components.menu.Menu;
import org.bosik.diacomp.web.frontend.wicket.components.menu.MenuContent;
import org.bosik.diacomp.web.frontend.wicket.components.menu.MenuItem;
import org.bosik.diacomp.web.frontend.wicket.pages.base.FoodBasePage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;
import org.bosik.diacomp.web.frontend.wicket.pages.login.LoginPage;
import org.bosik.diacomp.web.frontend.wicket.pages.stats.StatsPage;

public class MasterPage extends WebPage
{
	private static final long	serialVersionUID	= 1L;

	@SpringBean
	private UserInfoService		userInfoService;

	protected MasterPage()
	{
		super();
	}

	/**
	 * Shouldn't be called from browser directly
	 * 
	 * @param parameters
	 */
	protected MasterPage(PageParameters parameters)
	{
		super(parameters);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		try
		{
			String userName = userInfoService.getCurrentUserName();
			add(new Menu("menu", Model.of(getMenu(true, userName))));
			add(new AjaxClientInfoBehavior()
			{
				private static final long serialVersionUID = -4339758661303499417L;

				@Override
				protected void onClientInfo(AjaxRequestTarget target, WebClientInfo info)
				{
					super.onClientInfo(target, info);
				}
			});
		}
		catch (NotAuthorizedException e)
		{
			add(new Menu("menu", Model.of(getMenu(false, ""))));
		}

		add(new Label("pageTitle", getString("res.appTitle")));

		String buildTime = Config.get(Config.KEY_BUILD_TIME);
		String buildCommit = Config.get(Config.KEY_BUILD_COMMIT);
		String timeZone = getTimeZone().getDisplayName();
		String info = String.format("%s %s / %s", buildTime, buildCommit, timeZone);
		add(new Label("textVersion", info));
	}

	private MenuContent getMenu(boolean authorized, String userName)
	{
		MenuContent menuContent = new MenuContent();

		if (authorized)
		{
			menuContent.getItems().add(new MenuItem(getString("menu.diary"), DiaryPage.class));
			menuContent.getItems().add(new MenuItem(getString("menu.base"), FoodBasePage.class));
			menuContent.getItems().add(new MenuItem(getString("menu.stats"), StatsPage.class));
			//menuContent.getItems().add(new MenuItem(getString("menu.download"), DownloadPage.class));
		}
		else
		{
			menuContent.getItems().add(new MenuItem(getString("menu.login"), LoginPage.class));
		}

		menuContent.setSelected(getClass());
		menuContent.setUserName(userName);

		return menuContent;
	}

	private static ClientProperties getClientProperties(Component component)
	{
		IRequestCycleSettings requestCycleSettings = component.getApplication().getRequestCycleSettings();
		boolean gatherExtendedBrowserInfo = requestCycleSettings.getGatherExtendedBrowserInfo();
		ClientProperties properties = null;
		try
		{
			requestCycleSettings.setGatherExtendedBrowserInfo(false);
			WebClientInfo clientInfo = (WebClientInfo)component.getSession().getClientInfo();
			properties = clientInfo.getProperties();
		}
		finally
		{
			requestCycleSettings.setGatherExtendedBrowserInfo(gatherExtendedBrowserInfo);
		}
		return properties;
	}

	public static TimeZone getTimeZone(Component component)
	{
		ClientProperties properties = getClientProperties(component);
		TimeZone timeZone = properties.getTimeZone();
		if (timeZone != null)
		{
			return timeZone;
		}
		else
		{
			return TimeZone.getTimeZone("GMT");
		}
	}

	public TimeZone getTimeZone()
	{
		return getTimeZone(this);
	}
}
