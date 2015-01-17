package org.bosik.diacomp.android.utils;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import android.content.Context;
import android.util.Log;

public class FileWorker
{
	private static final String	TAG	= FileWorker.class.getSimpleName();
	private Context				context;

	public FileWorker(Context context)
	{
		this.context = context;
	}

	/**
	 * Проверяет, существует ли файл с указанным именем
	 * 
	 * @param fileName
	 *            Имя файла
	 * @return Существует ли файл
	 */
	public boolean fileExists(String fileName)
	{
		return context.getFileStreamPath(fileName).exists();
	}

	/**
	 * Читает содержимое файла в строку
	 * 
	 * @param fileName
	 *            Имя файла
	 * @return Содержимое файла в виде строки
	 * @throws IOException
	 */
	public String readFromFile(String fileName) throws IOException
	{
		/**/long time = System.currentTimeMillis();

		FileInputStream stream = context.openFileInput(fileName);
		InputStreamReader reader = new InputStreamReader(stream, "UTF-8");
		StringBuilder sb = new StringBuilder();
		try
		{
			BufferedReader bufferedReader = new BufferedReader(reader);

			String line;
			while ((line = bufferedReader.readLine()) != null)
			{
				sb.append(line);
			}
		}
		finally
		{
			reader.close();
			stream.close();
		}

		/**/Log.v(
				TAG,
				String.format("File '%s' read in %d msec (%d bytes)", fileName, System.currentTimeMillis() - time,
						sb.length()));
		return sb.toString();
	}

	/**
	 * Записывает строку в файл (исходное содержимое файла уничтожается).
	 * 
	 * @param fileName
	 *            Имя файла
	 * @param base
	 *            Строка
	 * @throws IOException
	 */
	public void writeToFile(String fileName, String data) throws IOException
	{
		/**/long time = System.currentTimeMillis();

		FileOutputStream outputStream = context.openFileOutput(fileName, Context.MODE_PRIVATE);
		try
		{
			outputStream.write(data.getBytes());
		}
		finally
		{
			outputStream.close();
		}

		/**/Log.v(TAG, String.format("File '%s' written in %d msec (%d bytes)", fileName, System.currentTimeMillis()
				- time, data.length()));
	}
}
