package org.bosik.diacomp.services.exceptions;

public class UnsupportedAPIException extends AuthException
{
	private static final long	serialVersionUID	= 1L;

	public UnsupportedAPIException(String msg)
	{
		super(msg);
	}
}
