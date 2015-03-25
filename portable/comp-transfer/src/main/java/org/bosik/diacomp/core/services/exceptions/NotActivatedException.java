package org.bosik.diacomp.core.services.exceptions;

public class NotActivatedException extends AuthException
{
	private static final long	serialVersionUID	= 1L;

	public NotActivatedException(String msg)
	{
		super(msg);
	}

	public NotActivatedException(Exception e)
	{
		super(e);
	}

	public NotActivatedException()
	{
		this("Not activated");
	}
}
