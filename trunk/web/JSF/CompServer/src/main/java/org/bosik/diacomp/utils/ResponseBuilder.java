package org.bosik.diacomp.utils;

public class ResponseBuilder
{
	public static final int	CODE_OK				= 0;
	public static final int	CODE_NOTFOUND		= 404;
	public static final int	CODE_FAIL			= 500;
	public static final int	CODE_BADCREDENTIALS	= 2;	// TODO
	public static final int	CODE_UNAUTHORIZED	= 401;

	public static String build(int code, String msg)
	{
		if (msg != null)
		{
			msg = msg.replace("\"", "\\\"");
		}

		return String.format("{code:%d, response:\"%s\"}", code, msg);
	}

	public static String buildDone(String msg)
	{
		return build(CODE_OK, msg);
	}

	public static String buildFails(String msg)
	{
		return build(CODE_FAIL, msg);
	}

	public static String buildNotAuthorized()
	{
		return build(CODE_UNAUTHORIZED, "Not authorized");
	}
}