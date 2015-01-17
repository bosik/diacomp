package org.bosik.diacomp.core.services.exceptions;

public class NotFoundException extends PersistenceException
{
	private static final long	serialVersionUID	= 1L;

	public NotFoundException(String id)
	{
		super(String.format("Item '%s' not found", id));
	}
}
