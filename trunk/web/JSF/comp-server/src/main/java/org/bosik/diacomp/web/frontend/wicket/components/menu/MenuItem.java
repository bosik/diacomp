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