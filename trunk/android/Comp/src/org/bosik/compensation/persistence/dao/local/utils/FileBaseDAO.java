package org.bosik.compensation.persistence.dao.local.utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.bosik.compensation.bo.common.Item;
import org.bosik.compensation.persistence.common.MemoryBase;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.utils.FileWorker;
import android.content.Context;

/**
 * Local file base
 * 
 * @author Bosik
 * 
 * @param <T>
 *            Type of base's item
 */
public class FileBaseDAO<T extends Item> implements BaseDAO<T>
{
	private MemoryBase<T>				base;
	private String						fileName;
	private Serializer<MemoryBase<T>>	serializer;
	private FileWorker					fileWorker;

	public FileBaseDAO(Context context, String fileName, Serializer<MemoryBase<T>> serializer) throws IOException
	{
		this.fileName = fileName;
		this.serializer = serializer;
		fileWorker = new FileWorker(context);

		if (fileWorker.fileExists(fileName))
		{
			load();
		}
	}

	@Override
	public String add(T item) throws BaseDAO.DuplicateException
	{
		if (base.get(item.getId()) == null)
		{
			base.add(item);
			save();
			return item.getId();
		}
		else
		{
			throw new DuplicateException(item.getId());
		}
	}

	@Override
	public void delete(String id) throws BaseDAO.ItemNotFoundException
	{
		if (base.remove(id))
		{
			save();
		}
		else
		{
			throw new ItemNotFoundException(id);
		}
	}

	@Override
	public List<T> findAll()
	{
		return base.getAll();
	}

	@Override
	public List<T> findAny(String filter)
	{
		List<T> result = new ArrayList<T>();
		filter = filter.toUpperCase();

		for (int i = 0; i < base.count(); i++)
		{
			T item = base.get(i);
			if (item.getName().toUpperCase().contains(filter))
			{
				result.add(item);
			}
		}

		return result;
	}

	@Override
	public T findOne(String exactName)
	{
		for (int i = 0; i < base.count(); i++)
		{
			T item = base.get(i);
			if (item.getName().equals(exactName))
			{
				return item;
			}
		}
		return null;
	}

	@Override
	public void replaceAll(List<T> newList, int newVersion)
	{
		base.clear();
		for (T item : newList)
		{
			base.add(item);
		}
		base.setVersion(newVersion);
		save();
	}

	@Override
	public void update(T item) throws BaseDAO.ItemNotFoundException
	{
		try
		{
			base.update(item);
			save();
		}
		catch (IndexOutOfBoundsException e)
		{
			throw new ItemNotFoundException(item.getId());
		}
	}

	@Override
	public int getVersion()
	{
		return base.getVersion();
	}

	// ----------------------------------- ОСТАЛЬНОЕ -----------------------------------

	private void load() throws IOException
	{
		String source = fileWorker.readFromFile(fileName);
		base = serializer.read(source);
	}

	public void save()
	{
		try
		{
			fileWorker.writeToFile(fileName, serializer.write(base));
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}
