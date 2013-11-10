package org.bosik.comp.web.Compensation.frontend;

import org.bosik.comp.web.Compensation.shared.FieldVerifier;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.Anchor;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.DialogBox;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.HeaderPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootLayoutPanel;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;

public class Compensation implements EntryPoint
{
	// Services
	private final GreetingServiceAsync	greetingService	= GWT.create(GreetingService.class);
	private final Localization			localization	= GWT.create(Localization.class);

	// Components
	private LoginInfo					loginInfo		= null;
	private VerticalPanel				loginPanel		= new VerticalPanel();

	private Label						loginLabel;
	private Anchor						signInLink;
	private Anchor						signOutLink;

	private HeaderPanel					mainPanel		= new HeaderPanel();
	private MyHeader					headerPanel		= new MyHeader("Компенсация");
	private VerticalPanel				contentPanel	= new VerticalPanel();

	/**
	 * This is the entry point method.
	 */
	@Override
	public void onModuleLoad()
	{
		// Check login status using login service.
		LoginServiceAsync loginService = GWT.create(LoginService.class);
		loginService.login(GWT.getHostPageBaseURL(), new AsyncCallback<LoginInfo>()
		{
			@Override
			public void onFailure(Throwable error)
			{
			}

			@Override
			public void onSuccess(LoginInfo result)
			{
				System.out.println("Auth info successfully received");

				loginInfo = result;

				// if (loginInfo.isLoggedIn())
				if (true)
				{
					loadCompensation();
				}
				else
				{
					loadLogin();
				}
			}
		});
	}

	private void loadLogin()
	{
		// Assemble login panel.
		signInLink = new Anchor(localization.labelLogin());
		signInLink.setHref(loginInfo.getLoginUrl());

		// loginPanel.add(new Label(localization.messageInfoLoginOffer()));
		loginPanel.add(signInLink);
		RootPanel.get("stockList").add(loginPanel);

		// RootPanel.get("stockList").setHeaderWidget(new MyHeader("Компенсация"));

		RootPanel.get("labelErrorNoJS").add(new Label(localization.messageErrorJsDisabled()));
	}

	private void loadCompensation()
	{
		// RootPanel.get("header").add(headerPanel);
		RootLayoutPanel.get().add(mainPanel);
		mainPanel.setWidth("500px");
		mainPanel.setStyleName("main");

		// RootPanel labelNoJS = RootPanel.get("labelErrorNoJS");
		// if (labelNoJS != null)
		// {
		// System.out.println("JS disabled!");
		// labelNoJS.add(new Label(localization.messageErrorJsDisabled()));
		// }
		// else
		// {
		// System.out.println("No NoJS label found");
		// }

		// ================================================
		// headerPanel.add(new MyHeader("The Title"));
		mainPanel.setHeaderWidget(headerPanel);
		mainPanel.setContentWidget(contentPanel);
		// RootPanel.get().add(new MyHeader("The Title"));
		// mainPanel.setHeaderWidget(headerPanel);

		// works
		// mainPanel.add(headerPanel);
		// mainPanel.add(contentPanel);
		// ================================================

		// Set up sign out hyperlink.
		signOutLink = new Anchor(localization.labelLogout());
		signOutLink.setHref(loginInfo.getLogoutUrl());
		// mainPanel.add(signOutLink);

		final Button sendButton = new Button("Send");
		sendButton.addStyleName("sendButton");
		final TextBox nameField = new TextBox();
		nameField.setText(loginInfo.getNickname());
		nameField.setFocus(true);
		nameField.selectAll();
		final Label welcomeLabel = new Label();
		welcomeLabel.setText("Hello!");
		welcomeLabel.setTitle("Tooltip");
		final Label errorLabel = new Label();

		// Add the nameField and sendButton to the RootPanel
		// Use RootPanel.get() to get the entire body element
		// RootPanel.get("labelWelcome").add(welcomeLabel);

		// RootPanel.get("nameFieldContainer").add(nameField);
		// RootPanel.get("sendButtonContainer").add(sendButton);
		// RootPanel.get("errorLabelContainer").add(errorLabel);
		contentPanel.add(nameField);
		contentPanel.add(sendButton);
		contentPanel.add(errorLabel);

		// Create the popup dialog box
		final DialogBox dialogBox = new DialogBox();
		dialogBox.setText("Remote Procedure Call");
		dialogBox.setAnimationEnabled(true);
		final Button closeButton = new Button("Close");
		// We can set the id of a widget by accessing its Element
		closeButton.getElement().setId("closeButton");
		final Label textToServerLabel = new Label();
		final HTML serverResponseLabel = new HTML();
		VerticalPanel dialogVPanel = new VerticalPanel();
		dialogVPanel.addStyleName("dialogVPanel");
		dialogVPanel.add(new HTML("<b>Sending name to the server:</b>"));
		dialogVPanel.add(textToServerLabel);
		dialogVPanel.add(new HTML("<br><b>Server replies:</b>"));
		dialogVPanel.add(serverResponseLabel);
		dialogVPanel.setHorizontalAlignment(VerticalPanel.ALIGN_RIGHT);
		dialogVPanel.add(closeButton);
		dialogBox.setWidget(dialogVPanel);

		// Add a handler to close the DialogBox
		closeButton.addClickHandler(new ClickHandler()
		{
			@Override
			public void onClick(ClickEvent event)
			{
				dialogBox.hide();
				sendButton.setEnabled(true);
				sendButton.setFocus(true);
			}
		});

		// Create a handler for the sendButton and nameField
		class MyHandler implements ClickHandler, KeyUpHandler
		{
			/**
			 * Fired when the user clicks on the sendButton.
			 */
			@Override
			public void onClick(ClickEvent event)
			{
				sendNameToServer();
			}

			/**
			 * Fired when the user types in the nameField.
			 */
			@Override
			public void onKeyUp(KeyUpEvent event)
			{
				if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER)
				{
					sendNameToServer();
				}
			}

			/**
			 * Send the name from the nameField to the server and wait for a response.
			 */
			private void sendNameToServer()
			{
				// First, we validate the input.
				errorLabel.setText("");
				String textToServer = nameField.getText();
				if (!FieldVerifier.isValidName(textToServer))
				{
					errorLabel.setText("Please enter at least four characters");
					return;
				}

				// Then, we send the input to the server.
				sendButton.setEnabled(false);
				textToServerLabel.setText(textToServer);
				serverResponseLabel.setText("");
				greetingService.greetServer(textToServer, new AsyncCallback<String>()
				{
					@Override
					public void onFailure(Throwable caught)
					{
						// Show the RPC error message to the user
						dialogBox.setText("Remote Procedure Call - Failure");
						serverResponseLabel.addStyleName("serverResponseLabelError");
						serverResponseLabel.setHTML(localization.messageErrorConnection());
						dialogBox.center();
						closeButton.setFocus(true);
					}

					@Override
					public void onSuccess(String result)
					{
						dialogBox.setText("Remote Procedure Call");
						serverResponseLabel.removeStyleName("serverResponseLabelError");
						serverResponseLabel.setHTML(result);
						dialogBox.center();
						closeButton.setFocus(true);
					}
				});
			}
		}

		// Add a handler to send the name to the server
		MyHandler handler = new MyHandler();
		sendButton.addClickHandler(handler);
		nameField.addKeyUpHandler(handler);

	}
}
