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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

public final class ZipUtils
{
	public static final String ENTRY_FILE_NAME = "data";

	private ZipUtils()
	{
		// to prevent instantiation
	}

	public static class Entry
	{
		private String name;
		private byte[] content;

		public Entry(String name, byte[] content)
		{
			this.name = name;
			this.content = content;
		}

		public String getName()
		{
			return name;
		}

		public void setName(String name)
		{
			this.name = name;
		}

		public byte[] getContent()
		{
			return content;
		}

		public void setContent(byte[] content)
		{
			this.content = content;
		}
	}

	public static InputStream zip(final List<Entry> entries) throws IOException
	{
		final PipedOutputStream sink = new PipedOutputStream();
		PipedInputStream source = new PipedInputStream(sink);

		new Thread(() -> {
			try (ZipOutputStream zip = new ZipOutputStream(sink))
			{
				for (Entry entry : entries)
				{
					zip.putNextEntry(new ZipEntry(entry.getName()));
					zip.write(entry.getContent());
					zip.closeEntry();
				}
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		}).start();

		return source;
	}

	public static List<Entry> unzip(InputStream stream) throws IOException
	{
		List<Entry> result = new ArrayList<>();

		try (ZipInputStream zipStream = new ZipInputStream(stream))
		{
			ZipEntry entry = zipStream.getNextEntry();
			while (entry != null)
			{
				try (ByteArrayOutputStream bufout = new ByteArrayOutputStream())
				{
					byte[] buffer = new byte[16 * 1024];
					int read;
					while ((read = zipStream.read(buffer)) != -1)
					{
						bufout.write(buffer, 0, read);
					}

					result.add(new Entry(entry.getName(), bufout.toByteArray()));
				}
				finally
				{
					zipStream.closeEntry();
				}

				entry = zipStream.getNextEntry();
			}
		}

		return result;
	}

	public static InputStream zipString(final String s) throws IOException
	{
		return zip(Collections.singletonList(new Entry(ENTRY_FILE_NAME, s.getBytes())));
	}

	public static String unzipString(InputStream stream) throws IOException
	{
		List<Entry> entries = unzip(stream);
		return entries.isEmpty() ? null : new String(entries.get(0).getContent(), "UTF-8");
	}
}
