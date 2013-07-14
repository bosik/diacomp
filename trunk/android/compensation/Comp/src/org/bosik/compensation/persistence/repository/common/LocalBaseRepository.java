package org.bosik.compensation.persistence.repository.common;

import java.io.IOException;
import android.content.Context;

/**
 * Локальный файловый репозиторий базы
 * 
 * @author Bosik
 * 
 * @param <BaseType>
 *            Тип базы
 * @param <Formatter>
 *            Класс, хранящий xml-formatter
 */
public class LocalBaseRepository<BaseType, Formatter extends BaseFormatter<BaseType>> extends FileRepository implements
		BaseRepository<BaseType>
{
	private Formatter formatter;

	public LocalBaseRepository(String fileName, Context context, Formatter formatter)
	{
		super(fileName, context);
		this.formatter = formatter;
	}

	// ============================== API ==============================

	@Override
	public int getVersion()
	{
		if (fileExists(fileName))
		{
			try
			{
				return formatter.getVersion(readFromFile(fileName));
			} catch (IOException e)
			{
				e.printStackTrace();
				return 0;
			}
		} else
		{
			return 0;
		}
	}

	@Override
	public BaseType getBase()
	{
		if (fileExists(fileName))
		{
			try
			{
				return formatter.read(readFromFile(fileName));
			} catch (IOException e)
			{
				e.printStackTrace();
				return null;
			}
		} else
		{
			return null;
		}
	}

	@Override
	public void postBase(BaseType base)
	{
		try
		{
			writeToFile(fileName, formatter.write(base));
		} catch (IOException e)
		{
			e.printStackTrace();
		}
	}
}
