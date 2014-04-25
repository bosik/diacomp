package org.bosik.diacomp.web.frontend.wicket.components.menu;

import org.apache.wicket.AttributeModifier;
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

		add(new AttributeModifier("class", "menu"));

		RepeatingView menu = new RepeatingView("menuItem");
		add(menu);

		for (int i = 0; i < getModelObject().getItems().size(); i++)
		{
			final MenuItem menuItem = Menu.this.getModelObject().getItems().get(i);

			BookmarkablePageLink<Void> link = new BookmarkablePageLink<Void>("Link", menuItem.getResponsePage());
			link.add(new Label("Text", menuItem.getCaption()));
			WebMarkupContainer itemPlace = new WebMarkupContainer(menu.newChildId());
			menu.add(itemPlace);
			itemPlace.add(link);

			if (i == getModelObject().getSelected())
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
