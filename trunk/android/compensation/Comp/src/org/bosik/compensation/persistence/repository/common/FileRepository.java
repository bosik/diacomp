package org.bosik.compensation.persistence.repository.common;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import android.content.Context;
import android.util.Log;

/**
 * Предоставляет методы для работы с файлами:
 * <ul>
 * <li>{@link #fileExists(fileName)}</li>
 * <li>{@link #readFromFile(fileName)}</li>
 * <li>{@link #writeToFile(fileName, data)}</li>
 * </ul>
 * Имя файла и контекст задаются при создании.
 * 
 * @author Bosik
 */
public class FileRepository
{
	private static final String TAG = FileRepository.class.getSimpleName();

	protected String fileName;
	protected Context context;

	public FileRepository(String fileName, Context context)
	{
		this.fileName = fileName;
		this.context = context;
	}

	// ============================== СЛУЖЕБНЫЕ ==============================

	/**
	 * Проверяет, существует ли файл с указанным именем
	 * 
	 * @param fileName
	 *            Имя файла
	 * @return Существует ли файл
	 */
	protected boolean fileExists(String fileName)
	{
		boolean result = context.getFileStreamPath(fileName).exists();

		/*
		 * if (result) Log.d(TAG, "File '" + fileName + "' exists"); else Log.d(TAG, "File '" +
		 * fileName + "' does not exist");
		 */

		return result;
	}

	/**
	 * Читает содержимое файла в строку
	 * 
	 * @param fileName
	 *            Имя файла
	 * @return Содержимое файла в виде строки
	 * @throws IOException
	 */
	protected String readFromFile(String fileName) throws IOException
	{
		FileInputStream stream = context.openFileInput(fileName);
		InputStreamReader reader = new InputStreamReader(stream, "UTF-8");
		BufferedReader bufferedReader = new BufferedReader(reader);
		StringBuilder sb = new StringBuilder();
		String line;
		while ((line = bufferedReader.readLine()) != null)
		{
			sb.append(line);
		}
		reader.close();

		// Log.d(TAG, "Reading from file '" + fileName + "': " + sb.toString());
		return sb.toString();

	}

	/**
	 * Записывает строку в файл (исходное содержимое файла уничтожается).
	 * 
	 * @param fileName
	 *            Имя файла
	 * @param data
	 *            Строка
	 * @throws IOException
	 */
	protected void writeToFile(String fileName, String data) throws IOException
	{
		// Log.d(TAG, "Writing to file '" + fileName + "': " + data);

		FileOutputStream outputStream = context.openFileOutput(fileName, Context.MODE_PRIVATE);
		outputStream.write(data.getBytes());
		outputStream.close();
	}
}
