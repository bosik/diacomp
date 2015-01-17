package org.bosik.diacomp.core.services.exceptions;

public class AlreadyDeletedException extends PersistenceException
{
	private static final long	serialVersionUID	= 1L;

	public AlreadyDeletedException(String id)
	{
		super(String.format("Item '%s' is already deleted", id));
	}
}
