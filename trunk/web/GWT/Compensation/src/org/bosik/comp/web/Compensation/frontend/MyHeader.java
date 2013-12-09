package org.bosik.comp.web.Compensation.frontend;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.ui.Anchor;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.DockLayoutPanel;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;

public class MyHeader extends Composite
{
	private final DockLayoutPanel	basePanel		= new DockLayoutPanel(Unit.PX);
	private final Label				labelTitle		= new Label();
	private final Image				image			= new Image();
	private final VerticalPanel		hints			= new VerticalPanel();
	private final Label				hintUsername	= new Label();
	private final Anchor			hintLink		= new Anchor();

	public MyHeader(String title, String userName, String linkText, String linkURL)
	{
		// initialization
		initWidget(basePanel);

		// constructing

		// logo
		image.setUrl("img/icon.jpg");
		image.setStyleName("icon");
		basePanel.addWest(image, 80); // XXX

		// hint box
		hints.setStyleName("login hint");
		basePanel.addEast(hints, 100); // XXX

		hintUsername.setText(userName);
		hints.add(hintUsername);

		hintLink.setText(linkText);
		hintLink.setHref(linkURL);
		hints.add(hintLink);

		// title
		labelTitle.setText(title);
		labelTitle.getElement().getStyle().setFontSize(1.5, Unit.EM);
		basePanel.add(labelTitle);

		// Styling
		setStyleName("title header");
	}
}
