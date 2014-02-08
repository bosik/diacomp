package org.bosik.diacomp.services.exceptions;

public class AuthException extends CommonServiceException
{
	private static final long	serialVersionUID	= 1L;

	public AuthException(String msg)
	{
		super(msg);
	}

	public AuthException(Exception e)
	{
		super(e);
	}
}
