package org.bosik.diacomp.web.frontend.wicket.components.menu;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class MenuContent implements Serializable
{
	private static final long		serialVersionUID	= 1L;

	private final List<MenuItem>	items				= new ArrayList<MenuItem>();
	private int						selected;

	public List<MenuItem> getItems()
	{
		return items;
	}

	public int getSelected()
	{
		return selected;
	}

	public void setSelected(int selected)
	{
		this.selected = selected;
	}
}
