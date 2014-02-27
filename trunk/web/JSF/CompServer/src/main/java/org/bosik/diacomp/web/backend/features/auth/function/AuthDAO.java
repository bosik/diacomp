package org.bosik.diacomp.web.backend.features.auth.function;

public interface AuthDAO
{
	int login(String login, String pass, int apiVersion);
}
