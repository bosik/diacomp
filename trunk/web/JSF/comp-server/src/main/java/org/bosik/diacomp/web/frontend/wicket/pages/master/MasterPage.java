package org.bosik.diacomp.web.frontend.wicket.pages.master;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.web.frontend.wicket.components.header.HeaderPanel;
import org.bosik.diacomp.web.frontend.wicket.components.menu.Menu;
import org.bosik.diacomp.web.frontend.wicket.components.menu.MenuContent;
import org.bosik.diacomp.web.frontend.wicket.components.menu.MenuItem;
import org.bosik.diacomp.web.frontend.wicket.pages.about.AboutPage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;
import org.bosik.diacomp.web.frontend.wicket.pages.download.DownloadPage;
import org.bosik.diacomp.web.frontend.wicket.pages.foodbase.FoodBasePage;

public class MasterPage extends WebPage
{
	private static final long	serialVersionUID	= 1L;

	public MasterPage(final PageParameters parameters)
	{
		super(parameters);

		add(new HeaderPanel("headerPanel", "user@user.com"));
		add(new Menu("menu", Model.of(getMenu())));
	}

	protected MenuContent getMenu()
	{
		MenuContent menuContent = new MenuContent();
		menuContent.getItems().add(new MenuItem("About", AboutPage.class));
		menuContent.getItems().add(new MenuItem("Diary", DiaryPage.class));
		menuContent.getItems().add(new MenuItem("Base", FoodBasePage.class));
		menuContent.getItems().add(new MenuItem("Download", DownloadPage.class));
		menuContent.setSelected(getClass());

		return menuContent;
	}
}
