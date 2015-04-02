/*  
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *  
 */
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
