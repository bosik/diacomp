package org.bosik.comp.web.Compensation.frontend;

import java.io.Serializable;

public class LoginInfo implements Serializable
{
	private static final long	serialVersionUID	= -5241839270336482448L;

	private boolean				loggedIn			= false;
	private String				loginUrl;
	private String				logoutUrl;
	private String				emailAddress;
	private String				nickname;

	public String getEmailAddress()
	{
		return emailAddress;
	}

	public String getLoginUrl()
	{
		return loginUrl;
	}

	public String getLogoutUrl()
	{
		return logoutUrl;
	}

	public String getNickname()
	{
		return nickname;
	}

	public boolean isLoggedIn()
	{
		return loggedIn;
	}

	public void setEmailAddress(String emailAddress)
	{
		this.emailAddress = emailAddress;
	}

	public void setLoggedIn(boolean loggedIn)
	{
		this.loggedIn = loggedIn;
	}

	public void setLoginUrl(String loginUrl)
	{
		this.loginUrl = loginUrl;
	}

	public void setLogoutUrl(String logoutUrl)
	{
		this.logoutUrl = logoutUrl;
	}

	public void setNickname(String nickname)
	{
		this.nickname = nickname;
	}
}
