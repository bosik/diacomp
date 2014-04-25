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

public class MasterPage extends WebPage
{
	private static final long	serialVersionUID	= 1L;

	public MasterPage(final PageParameters parameters)
	{
		super(parameters);

		add(new HeaderPanel("headerPanel", "user@user.com"));

		MenuContent menuContent = new MenuContent();
		menuContent.getItems().add(new MenuItem("Diary", DiaryPage.class));
		menuContent.getItems().add(new MenuItem("Base", DiaryPage.class)); // TODO
		menuContent.getItems().add(new MenuItem("About", AboutPage.class));
		menuContent.getItems().add(new MenuItem("Download", DiaryPage.class)); // TODO
		menuContent.setSelected(0);
		add(new Menu("menu", Model.of(menuContent)));
	}
}
