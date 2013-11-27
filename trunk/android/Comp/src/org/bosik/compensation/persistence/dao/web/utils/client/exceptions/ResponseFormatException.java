package org.bosik.compensation.persistence.dao.web.utils.client.exceptions;

/**
 * Incorrect server response format
 * 
 * @author Bosik
 */
public class ResponseFormatException extends WebClientException
{
	private static final long	serialVersionUID	= 6342429630144198560L;

	public ResponseFormatException(String detailMessage)
	{
		super(detailMessage);
	}

	public ResponseFormatException(Throwable throwable)
	{
		super(throwable);
	}

	public ResponseFormatException(String detailMessage, Throwable throwable)
	{
		super(detailMessage, throwable);
	}
}
