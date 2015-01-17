package org.bosik.diacomp.web.frontend.wicket.components.menu;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.apache.wicket.markup.html.WebPage;

public class MenuContent implements Serializable
{
	private static final long			serialVersionUID	= 1L;

	private final List<MenuItem>		items				= new ArrayList<MenuItem>();
	private Class<? extends WebPage>	selected;

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
}
