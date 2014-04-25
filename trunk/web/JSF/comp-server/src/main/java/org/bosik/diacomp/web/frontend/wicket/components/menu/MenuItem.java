package org.bosik.diacomp.web.frontend.wicket.components.menu;

import org.apache.wicket.markup.html.WebPage;

public class MenuItem
{
	private String	caption;
	private WebPage	responsePage;

	public String getCaption()
	{
		return caption;
	}

	public void setCaption(String caption)
	{
		this.caption = caption;
	}

	public WebPage getResponsePage()
	{
		return responsePage;
	}

	public void setResponsePage(WebPage responsePage)
	{
		this.responsePage = responsePage;
	}
}
