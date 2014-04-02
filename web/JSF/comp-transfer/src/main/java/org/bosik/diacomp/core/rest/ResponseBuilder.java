package org.bosik.diacomp.core.rest;

public class ResponseBuilder
{
	/**
	 * "Requested operation done OK"
	 */
	public static final int	CODE_OK					= 0;
	/**
	 * "The supplied username/password pair is invalid"
	 */
	public static final int	CODE_BADCREDENTIALS		= 401;	// TODO
	/**
	 * "You need to be authorized to invoke this method"
	 */
	public static final int	CODE_UNAUTHORIZED		= 401;	// TODO
	/**
	 * "The requested method / resource not found"
	 */
	public static final int	CODE_NOTFOUND			= 404;
	/**
	 * "This API version is deprecated, but still supported. Consider updating your client as soon as possible."
	 */
	public static final int	CODE_DEPRECATED_API		= 4050;
	/**
	 * "This API version is not supported anymore. Can't do anything."
	 */
	public static final int	CODE_UNSUPPORTED_API	= 4051;
	/**
	 * "Requested operation failed"
	 */
	public static final int	CODE_FAIL				= 500;

	public static String build(int code, String msg)
	{
		// if (msg != null)
		// {
		// msg = msg.replace("\"", "\\\"");
		// }
		//
		// return String.format("{code:%d, msg:\"%s\"}", code, msg);

		StdResponse resp = new StdResponse(code, msg);
		return StdResponse.encode(resp);
	}

	public static String buildDone(String msg)
	{
		return build(CODE_OK, msg);
	}

	public static String buildFails(String msg)
	{
		return build(CODE_FAIL, msg);
	}

	public static String buildFails()
	{
		return build(CODE_FAIL, "Internal error");
	}

	public static String buildNotAuthorized()
	{
		return build(CODE_UNAUTHORIZED, "Not authorized");
	}
}
