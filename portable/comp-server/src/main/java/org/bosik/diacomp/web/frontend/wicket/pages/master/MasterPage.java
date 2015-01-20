package org.bosik.diacomp.web.frontend.wicket.pages.master;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.auth.service.AuthService;
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
	private AuthService			authService;

	public MasterPage(final PageParameters parameters)
	{
		super(parameters);

		try
		{
			String userName = authService.getCurrentUserName();
			add(new HeaderPanel("headerPanel", userName));
			add(new Menu("menu", Model.of(getMenu(true))));
		}
		catch (NotAuthorizedException e)
		{
			add(new HeaderPanel("headerPanel"));
			add(new Menu("menu", Model.of(getMenu(false))));
		}

		add(new Label("pageTitle", getString("res.appTitle")));
	}

	protected MenuContent getMenu(boolean authorized)
	{
		// TODO: localize captions

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
