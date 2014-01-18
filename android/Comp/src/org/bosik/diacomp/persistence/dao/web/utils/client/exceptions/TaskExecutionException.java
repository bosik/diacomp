package org.bosik.diacomp.persistence.dao.web.utils.client.exceptions;

public class TaskExecutionException extends WebClientException
{
	private static final long	serialVersionUID	= 1010290887263189691L;

	public TaskExecutionException(int code, String detailMessage)
	{
		super(String.format("Error %d: %s", code, detailMessage));
	}
}
