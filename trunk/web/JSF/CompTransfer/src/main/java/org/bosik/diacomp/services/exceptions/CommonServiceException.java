package org.bosik.diacomp.services.exceptions;

public class CommonServiceException extends RuntimeException
{
	private static final long	serialVersionUID	= 1L;

	public CommonServiceException(String msg)
	{
		super(msg);
	}

	public CommonServiceException(Exception e)
	{
		super(e);
	}

	// public CommonServiceException()
	// {
	// super();
	// }
}
