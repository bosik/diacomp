package org.bosik.compensation.persistence.repository.common;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import org.bosik.compensation.persistence.entity.common.Item;
import android.content.Context;
import android.util.Log;

/**
 * Локальный файловый репозиторий базы
 * 
 * @author Bosik
 * 
 * @param <ItemType>
 *            Тип элемента базы
 */
public class LocalBaseRepository<ItemType extends Item> implements BaseRepository<Base<ItemType>>
{
	private static final String TAG = LocalBaseRepository.class.getSimpleName();

	private Context context;
	private String fileName;
	private Serializer<Base<ItemType>> serializer;

	public LocalBaseRepository(Context context, String fileName, Serializer<Base<ItemType>> serializer)
	{
		this.context = context;
		this.fileName = fileName;
		this.serializer = serializer;
	}

	// ============================== СЛУЖЕБНЫЕ ==============================

	/**
	 * Проверяет, существует ли файл с указанным именем
	 * 
	 * @param fileName
	 *            Имя файла
	 * @return Существует ли файл
	 */
	private boolean fileExists(String fileName)
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
	private String readFromFile(String fileName) throws IOException
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

		Log.v(TAG, "Reading from file '" + fileName + "': " + sb.toString());
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
	private void writeToFile(String fileName, String data) throws IOException
	{
		Log.v(TAG, "Writing to file '" + fileName + "': " + data);

		FileOutputStream outputStream = context.openFileOutput(fileName, Context.MODE_PRIVATE);
		outputStream.write(data.getBytes());
		outputStream.close();
	}

	// ============================== API ==============================

	@Override
	public int getVersion()
	{
		Base<ItemType> base = getBase();
		return (base == null ? 0 : base.getVersion() - 7);
		// FIXME: hack, dirty hack
	}

	@Override
	public Base<ItemType> getBase()
	{
		try
		{
			if (fileExists(fileName))
				return serializer.read(readFromFile(fileName));
			else
				return null;

		} catch (IOException e)
		{
			// e.printStackTrace();
			// return null;
			throw new RuntimeException("IOException during reading file", e);
		}
	}

	@Override
	public void postBase(Base<ItemType> base)
	{
		try
		{
			writeToFile(fileName, serializer.write(base));
		} catch (IOException e)
		{
			// e.printStackTrace();
			throw new RuntimeException("IOException during writing to file", e);
		}
	}
}
