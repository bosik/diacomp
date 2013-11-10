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
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class Compensation implements EntryPoint
{
	// String resources
	// private static final String MESSAGE_ERROR_CONNECTION =
	// "При подключении возникла ошибка. Повторите попытку.";
	private static final String			MESSAGE_ERROR_JS_DISABLED	= "Для корректной работы приложения ваш браузер должен выполнять Java Script";
	private static final String			LABEL_LOGIN_OFFER			= "Войдите с помощью учётной записи Google";
	private static final String			LABEL_SIGN_IN				= "Вход";
	private static final String			LABEL_SIGN_OUT				= "Выход";

	// Services
	private final GreetingServiceAsync	greetingService				= GWT.create(GreetingService.class);
	private final Localization			localization				= GWT.create(Localization.class);

	// Components
	private LoginInfo					loginInfo					= null;
	private VerticalPanel				loginPanel					= new VerticalPanel();
	private VerticalPanel				mainPanel					= new VerticalPanel();
	private Label						loginLabel					= new Label(LABEL_LOGIN_OFFER);
	private Anchor						signInLink					= new Anchor(LABEL_SIGN_IN);
	private Anchor						signOutLink					= new Anchor(LABEL_SIGN_OUT);

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
				loginInfo = result;
				if (loginInfo.isLoggedIn())
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
		signInLink.setHref(loginInfo.getLoginUrl());
		loginPanel.add(loginLabel);
		loginPanel.add(signInLink);
		RootPanel.get("stockList").add(loginPanel);

	}

	private void loadCompensation()
	{
		RootPanel.get("labelErrorNoJS").add(new Label(MESSAGE_ERROR_JS_DISABLED));

		// Set up sign out hyperlink.
		signOutLink.setHref(loginInfo.getLogoutUrl());

		System.out.println("Logout URL: " + loginInfo.getLogoutUrl());

		final Button sendButton = new Button("Send");
		final TextBox nameField = new TextBox();
		nameField.setText(loginInfo.getNickname());
		final Label welcomeLabel = new Label();
		final Label errorLabel = new Label();

		// We can add style names to widgets
		sendButton.addStyleName("sendButton");

		// Add the nameField and sendButton to the RootPanel
		// Use RootPanel.get() to get the entire body element
		RootPanel.get("labelWelcome").add(welcomeLabel);
		RootPanel.get("nameFieldContainer").add(nameField);
		RootPanel.get("sendButtonContainer").add(sendButton);
		RootPanel.get("errorLabelContainer").add(errorLabel);

		welcomeLabel.setText("Hello!");
		welcomeLabel.setTitle("Tooltip");

		// Focus the cursor on the name field when the app loads
		nameField.setFocus(true);
		nameField.selectAll();

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

		mainPanel.add(signOutLink);
		RootPanel.get("stockList").add(mainPanel);
	}

	// public String getUserNickname()
	// {
	// User user = UserServiceFactory.getUserService().getCurrentUser();
	// if (user == null)
	// {
	// return "null";
	// }
	// else
	// {
	// return user.getNickname();
	// }
	// }
}
