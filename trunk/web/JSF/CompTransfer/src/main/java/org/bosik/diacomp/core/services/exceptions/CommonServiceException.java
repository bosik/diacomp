package org.bosik.diacomp.core.services.exceptions;

public class CommonServiceException extends RuntimeException
{
	private static final long	serialVersionUID	= 1L;

	public CommonServiceException(String msg)
	{
		super(msg);
	}

	public CommonServiceException(Throwable e)
	{
		super(e);
	}

	public CommonServiceException(String msg, Throwable e)
	{
		super(msg, e);
	}
}
