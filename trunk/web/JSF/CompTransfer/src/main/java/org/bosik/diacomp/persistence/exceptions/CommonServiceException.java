package org.bosik.diacomp.persistence.exceptions;

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
}
