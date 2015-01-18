package org.bosik.diacomp.core.services.exceptions;

public class TooManyItemsException extends CommonServiceException
{
	private static final long	serialVersionUID	= 1L;

	public TooManyItemsException(String msg)
	{
		super(msg);
	}

	public TooManyItemsException(Exception e)
	{
		super(e);
	}
}