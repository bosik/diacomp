package org.bosik.compensation.persistence.exceptions;

public class AlreadyDeletedException extends CommonDAOException
{
	private static final long	serialVersionUID	= 1L;

	public AlreadyDeletedException(String id)
	{
		super(String.format("Item '%s' is already deleted", id));
	}
}
