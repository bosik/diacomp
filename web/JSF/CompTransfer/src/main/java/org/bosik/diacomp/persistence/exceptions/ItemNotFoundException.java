package org.bosik.diacomp.persistence.exceptions;

public class ItemNotFoundException extends CommonServiceException
{
	private static final long	serialVersionUID	= 1L;

	public ItemNotFoundException(String id)
	{
		super(String.format("Item '%s' not found", id));
	}
}
