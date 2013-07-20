package org.bosik.compensation.persistence.repository.common;

import java.io.IOException;
import org.bosik.compensation.persistence.entity.common.CustomBase;
import android.content.Context;

/**
 * Локальный файловый репозиторий базы
 * 
 * @author Bosik
 * 
 * @param <BaseType>
 *            Тип базы
 * @param <Serial>
 *            Сериализатор
 */
public class LocalBaseRepository<BaseType extends CustomBase, Serial extends Serializer<BaseType>> extends
		FileRepository implements BaseRepository<BaseType>
{
	private Serial serializer;

	public LocalBaseRepository(String fileName, Context context, Serial formatter)
	{
		super(fileName, context);
		this.serializer = formatter;
	}

	// ============================== API ==============================

	@Override
	public int getVersion()
	{
		if (fileExists(fileName))
		{
			try
			{
				BaseType base = serializer.read(readFromFile(fileName));
				return base.getVersion();
				// return serializer.getVersion(readFromFile(fileName));

				// TODO: cleanup; think about optimization if need
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
				return serializer.read(readFromFile(fileName));
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
			writeToFile(fileName, serializer.write(base));
		} catch (IOException e)
		{
			e.printStackTrace();
		}
	}
}
