package org.bosik.compensation.persistence.repository.common;

import java.io.IOException;
import org.bosik.compensation.persistence.entity.common.Item;
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
public class LocalBaseRepository<ItemType extends Item> implements BaseRepository<Base<ItemType>>
{
	private FileRepository fileRepository;
	private Serializer<Base<ItemType>> serializer;
	private String fileName;

	public LocalBaseRepository(Context context, String fileName, Serializer<Base<ItemType>> serializer)
	{
		this.fileRepository = new FileRepository(context);
		this.fileName = fileName;
		this.serializer = serializer;
	}

	// ============================== API ==============================

	@Override
	public int getVersion()
	{
		if (fileRepository.fileExists(fileName))
		{
			try
			{
				Base<ItemType> base = serializer.read(fileRepository.readFromFile(fileName));
				return base.getVersion();
				// TODO: think about optimization if need
			} catch (IOException e)
			{
				// e.printStackTrace();
				// return 0;
				throw new RuntimeException("IOException during reading file", e);
			}
		} else
		{
			return 0;
		}
	}

	@Override
	public Base<ItemType> getBase()
	{
		if (fileRepository.fileExists(fileName))
		{
			try
			{
				return serializer.read(fileRepository.readFromFile(fileName));
			} catch (IOException e)
			{
				// e.printStackTrace();
				// return null;
				throw new RuntimeException("IOException during reading file", e);
			}
		} else
		{
			return null;
		}
	}

	@Override
	public void postBase(Base<ItemType> base)
	{
		try
		{
			fileRepository.writeToFile(fileName, serializer.write(base));
		} catch (IOException e)
		{
			// e.printStackTrace();
			throw new RuntimeException("IOException during writing to file", e);
		}
	}
}
