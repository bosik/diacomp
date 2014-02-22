package org.bosik.diacomp.android.persistence.services.web.utils.client.exceptions;

/**
 * The subject
 * 
 * @author Bosik
 */
@Deprecated
public class DeprecatedAPIException extends WebClientException
{
	private static final long	serialVersionUID	= -4897188574347397921L;

	/*
	 * public UnsupportedAPIException(String clientApiVersion, String serverApiVersion) {
	 * super("Client API version (" + clientApiVersion + ") is deprecated, server required version "
	 * + serverApiVersion); }
	 */
	public DeprecatedAPIException(String detailMessage)
	{
		super(detailMessage);
	}
}
