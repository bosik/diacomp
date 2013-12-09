package org.bosik.comp.web.Compensation.frontend;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.Command;
import com.google.gwt.user.client.ui.Anchor;
import com.google.gwt.user.client.ui.CaptionPanel;
import com.google.gwt.user.client.ui.DockLayoutPanel;
import com.google.gwt.user.client.ui.HasHorizontalAlignment;
import com.google.gwt.user.client.ui.HasVerticalAlignment;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Hyperlink;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.MenuBar;
import com.google.gwt.user.client.ui.MenuItem;
import com.google.gwt.user.client.ui.MenuItemSeparator;
import com.google.gwt.user.client.ui.RootLayoutPanel;
import com.google.gwt.user.client.ui.TabLayoutPanel;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.datepicker.client.DatePicker;

public class Compensation implements EntryPoint
{
	// Services
	private final GreetingServiceAsync	greetingService	= GWT.create(GreetingService.class);
	private final Localization			localization	= GWT.create(Localization.class);

	// Components
	private LoginInfo					loginInfo		= null;
	private VerticalPanel				loginPanel		= new VerticalPanel();
	private Anchor						signInLink;

	/**
	 * This is the entry point method.
	 */
	@Override
	public void onModuleLoad()
	{
		// Check login status using login service.
		// LoginServiceAsync loginService = GWT.create(LoginService.class);
		// loginService.login(GWT.getHostPageBaseURL(), new AsyncCallback<LoginInfo>()
		// {
		// @Override
		// public void onFailure(Throwable error)
		// {
		// }
		//
		// @Override
		// public void onSuccess(LoginInfo result)
		// {
		// System.out.println("Auth info successfully received");
		//
		// loginInfo = result;
		//
		// loginInfo.setNickname("Demo Nickname");
		//
		// // if (loginInfo.isLoggedIn())
		// if (true)
		// {
		// //
		// }
		// else
		// {
		// loadLogin();
		// }
		// }
		// });

		loginInfo = makeDummyLoginInfo();
		loadCompensation();
	}

	private LoginInfo makeDummyLoginInfo()
	{
		LoginInfo info = new LoginInfo();
		info.setEmailAddress("demo@mail.com");
		info.setLoggedIn(true);
		info.setNickname("Nick");
		return info;
	}

	private void loadLogin()
	{
		// Assemble login panel.
		// signInLink = new Anchor(localization.labelLogin());
		// signInLink.setHref(loginInfo.getLoginUrl());

		// loginPanel.add(new Label(localization.messageInfoLoginOffer()));
		// loginPanel.add(signInLink);
		// RootPanel.get("stockList").add(loginPanel);

		// RootPanel.get("stockList").setHeaderWidget(new MyHeader("Компенсация"));

		// RootPanel.get("labelErrorNoJS").add(new Label(localization.messageErrorJsDisabled()));
	}

	private void loadCompensation()
	{
		RootLayoutPanel rootLayoutPanel = RootLayoutPanel.get();
		rootLayoutPanel.setSize("1000px", "500px");

		VerticalPanel mainPanel = new VerticalPanel();
		rootLayoutPanel.add(mainPanel);
		mainPanel.setSize("100%", "100%");

		DockLayoutPanel headerPanel = new DockLayoutPanel(Unit.EM);
		mainPanel.add(headerPanel);
		mainPanel.setCellVerticalAlignment(headerPanel, HasVerticalAlignment.ALIGN_MIDDLE);
		headerPanel.setSize("100%", "87px");

		Image image = new Image("img/icon.jpg");
		headerPanel.addWest(image, 5.4);
		image.setSize("32", "32");

		Label lblNewLabel = new Label("Компенсация");
		lblNewLabel.setStyleName("header");
		headerPanel.addWest(lblNewLabel, 10.0);
		lblNewLabel.setHeight("100%");

		VerticalPanel verticalPanel = new VerticalPanel();
		verticalPanel.setHorizontalAlignment(HasHorizontalAlignment.ALIGN_RIGHT);
		headerPanel.addEast(verticalPanel, 11.5);
		verticalPanel.setHeight("100%");

		Label label = new Label("bosiknk@rambler.ru");
		verticalPanel.add(label);

		Hyperlink hprlnkNewHyperlink = new Hyperlink("Выход", false, "newHistoryToken");
		verticalPanel.add(hprlnkNewHyperlink);

		VerticalPanel contentPanel = new VerticalPanel();
		mainPanel.add(contentPanel);
		contentPanel.setWidth("100%");

		MenuBar menuBar = new MenuBar(false);
		contentPanel.add(menuBar);
		menuBar.setWidth("100%");
		MenuBar menuBar_1 = new MenuBar(true);

		MenuItem mntmNewMenu = new MenuItem("Файл", false, menuBar_1);

		MenuItem mntmNewItem_1 = new MenuItem("Настройки...", false, (Command) null);
		menuBar_1.addItem(mntmNewItem_1);
		MenuBar menuBar_2 = new MenuBar(true);

		MenuItem mntmNewMenu_1 = new MenuItem("Экспорт", false, menuBar_2);

		MenuItem mntmNewItem = new MenuItem("JSON...", false, (Command) null);
		menuBar_2.addItem(mntmNewItem);

		MenuItem mntmNewItem_3 = new MenuItem("TXT...", false, (Command) null);
		menuBar_2.addItem(mntmNewItem_3);
		menuBar_1.addItem(mntmNewMenu_1);

		MenuItemSeparator separator = new MenuItemSeparator();
		menuBar_1.addSeparator(separator);

		MenuItem mntmNewItem_2 = new MenuItem("Выход", false, (Command) null);
		menuBar_1.addItem(mntmNewItem_2);
		menuBar.addItem(mntmNewMenu);

		TabLayoutPanel tabLayoutPanel = new TabLayoutPanel(1.5, Unit.EM);
		tabLayoutPanel.setAnimationDuration(300);

		HorizontalPanel horizontalPanel = new HorizontalPanel();
		tabLayoutPanel.add(horizontalPanel, "Дневник", false);
		horizontalPanel.setSize("100%", "100%");

		DatePicker datePicker = new DatePicker();
		horizontalPanel.add(datePicker);

		CaptionPanel captionPanel = new CaptionPanel("Статистика");
		horizontalPanel.add(captionPanel);
		captionPanel.setSize("100%", "187px");

		VerticalPanel verticalPanel_1 = new VerticalPanel();
		tabLayoutPanel.add(verticalPanel_1, "База продуктов", false);
		verticalPanel_1.setHeight("57px");

		VerticalPanel verticalPanel_2 = new VerticalPanel();
		tabLayoutPanel.add(verticalPanel_2, "База блюд", false);
		contentPanel.add(tabLayoutPanel);
		tabLayoutPanel.setSize("100%", "500px");
		// RootPanel.get().add(new MyHeader("The Title"));
		// mainPanel.setHeaderWidget(headerPanel);

		// works
		// mainPanel.add(headerPanel);
		// mainPanel.add(contentPanel);
		// ================================================
	}
}
