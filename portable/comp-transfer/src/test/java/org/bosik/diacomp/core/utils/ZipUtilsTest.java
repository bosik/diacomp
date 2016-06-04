/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.utils;

import java.io.IOException;
import java.io.InputStream;
import org.junit.Test;
import junit.framework.TestCase;

@SuppressWarnings("static-method")
public class ZipUtilsTest extends TestCase
{
	@Test
	public void testZipUnzipString() throws IOException
	{
		final String s = "some secret string";

		InputStream stream = ZipUtils.zipString(s);
		String result = ZipUtils.unzipString(stream);

		assertEquals(s, result);
	}
}
