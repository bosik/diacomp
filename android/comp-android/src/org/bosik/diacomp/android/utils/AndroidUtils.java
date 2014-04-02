package org.bosik.diacomp.android.utils;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;

public class AndroidUtils
{
	// отладочная печать
	// private static final String TAG = "AndroidUtils";

	public static String Cp1251ToUtf8(String s)
	{
		// got no idea why it works
		return s;

		/*
		 * try { //return new String(s.getBytes(), "UTF-8"); } catch (Exception e) {
		 * e.printStackTrace(); throw new RuntimeException("Unsupported code page", e); }
		 */

		/*
		 * Charset charset = Charset.forName("Cp1251"); CharsetDecoder decoder =
		 * charset.newDecoder(); CharsetEncoder encoder = charset.newEncoder();
		 *
		 * try { ByteBuffer bbuf = decoder.decode(CharBuffer.wrap(s)); CharBuffer cbuf =
		 * encoder.encode(bbuf); return cbuf.toString(); } catch (CharacterCodingException e) {
		 * throw new RuntimeException(e); }
		 */
	}

	public static String Utf8ToCp1251(String s)
	{
		/*
		 * try { return new String(s.getBytes(), "Cp1251"); } catch (UnsupportedEncodingException e)
		 * { e.printStackTrace(); throw new RuntimeException("Unsupported code page", e); }
		 */

		Charset charset = Charset.forName("Cp1251");
		CharsetDecoder decoder = charset.newDecoder();
		CharsetEncoder encoder = charset.newEncoder();

		decoder.onMalformedInput(CodingErrorAction.IGNORE);
		encoder.onMalformedInput(CodingErrorAction.IGNORE);
		try
		{
			ByteBuffer bbuf = encoder.encode(CharBuffer.wrap(s));
			CharBuffer cbuf = decoder.decode(bbuf);
			return cbuf.toString();
		}
		catch (CharacterCodingException e)
		{
			throw new RuntimeException(e);
		}
	}
}
