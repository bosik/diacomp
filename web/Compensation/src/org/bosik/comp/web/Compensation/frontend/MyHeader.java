package org.bosik.comp.web.Compensation.frontend;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;

public class MyHeader extends Composite
{
	private VerticalPanel	basePanel;
	private Label			labelTitle	= new Label();
	final Image				image		= new Image();

	public MyHeader(String title)
	{
		// constructing
		basePanel = new VerticalPanel();

		image.setUrl("img/icon.jpg");
		basePanel.add(image);

		labelTitle.setText(title);
		labelTitle.getElement().getStyle().setFontSize(2, Unit.EM);
		basePanel.add(labelTitle);

		// initialization
		initWidget(basePanel);

		// Styling
		// setStyleName("title");
	}
}
