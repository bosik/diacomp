package org.bosik.diacomp.services.exceptions;

public class NotAuthorizedException extends AuthException
{
	private static final long	serialVersionUID	= 1L;

	public NotAuthorizedException(String msg)
	{
		super(msg);
	}

	public NotAuthorizedException(Exception e)
	{
		super(e);
	}

	public NotAuthorizedException()
	{
		this("Not authorized");
	}
}
