package org.bosik.comp.web.Compensation.frontend;

import java.util.Map;
import com.google.gwt.i18n.client.Constants;

public interface Localization extends Constants
{
	Map<String, String> colorMap();

	@Key("message.error.connection")
	String messageErrorConnection();

	@Key("message.error.js_disabled")
	String messageErrorJsDisabled();

	@Key("message.info.login_offer")
	String messageInfoLoginOffer();

	@Key("label.login")
	String labelLogin();

	@Key("label.logout")
	String labelLogout();
}
