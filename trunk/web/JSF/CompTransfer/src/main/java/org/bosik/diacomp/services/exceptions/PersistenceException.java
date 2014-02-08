package org.bosik.diacomp.services.exceptions;

public class PersistenceException extends CommonServiceException
{
	private static final long	serialVersionUID	= 1L;

	public PersistenceException(String msg)
	{
		super(msg);
	}

	public PersistenceException(Exception e)
	{
		super(e);
	}
}