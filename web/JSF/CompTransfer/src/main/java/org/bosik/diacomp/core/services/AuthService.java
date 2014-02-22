package org.bosik.diacomp.core.services;

public interface AuthService
{
	void login(String login, String pass, int apiVersion);

	void logout();
}
