package org.bosik.compensation.persistence.exceptions;

public class CommonDAOException extends RuntimeException
{
	private static final long	serialVersionUID	= 1L;

	public CommonDAOException(String msg)
	{
		super(msg);
	}

	public CommonDAOException(Exception e)
	{
		super(e);
	}
}
