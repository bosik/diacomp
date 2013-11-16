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
 * Локальная файловая база
 * 
 * @author Bosik
 * 
 * @param <ItemType>
 *            Тип элемента базы
 */
public class LocalBase<ItemType extends Item> extends Base<ItemType> implements Interchangeable
{
	private static final String			TAG	= LocalBase.class.getSimpleName();

	private Context						context;
	private String						fileName;
	private Serializer<Base<ItemType>>	serializer;
	private int							fileVersion;

	public LocalBase(Context context, String fileName, Serializer<Base<ItemType>> serializer)
	{
		this.context = context;
		this.fileName = fileName;
		this.serializer = serializer;

		if (fileExists(fileName))
		{
			load();
		}

		fileVersion = getVersion();
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

	// ============================== ОТКРЫТЫЕ ==============================

	// ----------------------------------- API -----------------------------------

	@Override
	public void read(String data)
	{
		serializer.read(this, data);
	}

	@Override
	public String write()
	{
		return serializer.write(this);
	}

	// public int getVersion()
	// {
	// Base<ItemType> base = getBase();
	// return (base == null ? 0 : base.getVersion());
	// }

	// @Override
	// public Base<ItemType> getBase()
	// {
	// try
	// {
	// if (fileExists(fileName))
	// return serializer.read(readFromFile(fileName));
	// else
	// return null;
	//
	// } catch (IOException e)
	// {
	// throw new RuntimeException(e);
	// }
	// }

	// ----------------------------------- ОСТАЛЬНОЕ -----------------------------------

	public void load()
	{
		try
		{
			// if (fileExists(fileName))
			// {
			read(readFromFile(fileName));
			fileVersion = getVersion();
			// } else
			// {
			// might be called from constructor, so appropriative file might not exists — it's
			// ok
			// THINK: else what? clear/ignore?
			// }
		}
		catch (Exception e)
		{
			// throw new RuntimeException(e);
			// TODO: ignore throw
		}
	}

	// @Override
	// public void postBase(Base<ItemType> base)
	// {
	// try
	// {
	// writeToFile(fileName, serializer.write(base));
	// } catch (IOException e)
	// {
	// throw new RuntimeException(e);
	// }
	// }

	public void save()
	{
		try
		{
			writeToFile(fileName, write());
			fileVersion = getVersion();
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}

	public boolean modified()
	{
		return getVersion() > fileVersion;
	}
}
