package org.bosik.compensation.persistence.repository.common;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import android.content.Context;

/**
 * Предоставляет методы для работы с файлами (проверка существования, загрузка в строку, сохранение
 * из строки). Имя файла и контекст задаются при создании.
 * 
 * @author Bosik
 * 
 */
public class FileRepository
{
	protected String fileName;
	protected Context context;

	public FileRepository(String fileName, Context context)
	{
		this.fileName = fileName;
		this.context = context;
		// getBaseContext()
	}

	// ============================== СЛУЖЕБНЫЕ ==============================

	protected boolean fileExists(String fileName)
	{
		return context.getFileStreamPath(fileName).exists();
	}

	protected String readFromFile(String fileName) throws IOException
	{
		FileInputStream stream = new FileInputStream(fileName);
		StringBuilder sb = new StringBuilder();
		Reader r = new InputStreamReader(stream, "UTF-8");
		int ch = r.read();
		while (ch >= 0)
		{
			sb.append(ch);
			ch = r.read();
		}
		r.close();
		return sb.toString();
	}

	protected void writeToFile(String fileName, String data) throws IOException
	{
		OutputStreamWriter outputStreamWriter = new OutputStreamWriter(context.openFileOutput(fileName, Context.MODE_PRIVATE));
		outputStreamWriter.write(data);
		outputStreamWriter.close();
	}
}
