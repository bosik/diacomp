package org.bosik.diacomp.web.backend.features.auth.dao;

public interface AuthDAO
{
	int login(String login, String pass, int apiVersion);
}
