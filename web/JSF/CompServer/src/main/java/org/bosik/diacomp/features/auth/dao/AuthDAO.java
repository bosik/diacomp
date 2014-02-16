package org.bosik.diacomp.features.auth.dao;

public interface AuthDAO
{
	int login(String login, String pass, int apiVersion);
}
