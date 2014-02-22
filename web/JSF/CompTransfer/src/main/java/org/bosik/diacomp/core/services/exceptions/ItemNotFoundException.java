package org.bosik.diacomp.core.services.exceptions;

public class ItemNotFoundException extends PersistenceException
{
	private static final long	serialVersionUID	= 1L;

	public ItemNotFoundException(String id)
	{
		super(String.format("Item '%s' not found", id));
	}
}
