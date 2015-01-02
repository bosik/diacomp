package org.bosik.diacomp.web.frontend.wicket.components.menu;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;

public class Menu extends GenericPanel<MenuContent>
{
	private static final long	serialVersionUID	= 1L;

	public Menu(String id, IModel<MenuContent> content)
	{
		super(id);
		setModel(content);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

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
				link.add(new AttributeAppender("class", " menu_selected"));
			}
			else
			{
				link.add(new AttributeAppender("class", " menu_option"));
			}
		}
	}
}
