package org.bosik.compensation.persistence.dao.local.utils;

import java.io.IOException;
import java.util.List;
import org.bosik.compensation.persistence.common.MemoryBase;
import org.bosik.compensation.persistence.common.UniqueNamed;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.utils.FileWorker;
import android.content.Context;
import android.util.Log;

/**
 * Local file base
 * 
 * @author Bosik
 * 
 * @param <T>
 *            Type of base's item
 */
public class FileBaseDAO<T extends UniqueNamed> implements BaseDAO<T>
{
	private static final String			TAG	= FileBaseDAO.class.getSimpleName();

	private MemoryBase<T>				base;
	private String						fileName;
	private Serializer<MemoryBase<T>>	serializer;
	private FileWorker					fileWorker;

	public FileBaseDAO(Context context, String fileName, Serializer<MemoryBase<T>> serializer) throws IOException
	{
		this.fileName = fileName;
		this.serializer = serializer;
		fileWorker = new FileWorker(context);

		// fileWorker.writeToFile(fileName, "");

		load();
	}

	@Override
	public String add(T item) throws DuplicateException
	{
		base.add(item);
		save();
		return item.getId();
	}

	@Override
	public void delete(String id) throws ItemNotFoundException
	{
		base.remove(id);
	}

	@Override
	public List<T> findAll()
	{
		return base.findAll();
	}

	@Override
	public List<T> findAny(String filter)
	{
		return base.findAny(filter);
	}

	@Override
	public T findById(String id)
	{
		return base.findById(id);
	}

	@Override
	public T findOne(String exactName)
	{
		return base.findOne(exactName);
	}

	@Override
	public void replaceAll(List<T> newList, int newVersion)
	{
		base.replaceAll(newList, newVersion);
		save();
	}

	@Override
	public void update(T item) throws ItemNotFoundException
	{
		base.update(item);
		save();
	}

	@Override
	public int getVersion()
	{
		return base.getVersion();
	}

	// ----------------------------------- File I/O -----------------------------------

	private void load() throws IOException
	{
		if (fileWorker.fileExists(fileName))
		{
			String source = fileWorker.readFromFile(fileName);
			base = serializer.read(source);
			Log.v(TAG, String.format("Memory base \"%s\" loaded, total items: %d", fileName, base.count()));
		}
		else
		{
			base = new MemoryBase<T>();
			Log.w(TAG, String.format("Failed to load memory base \"%s\": file not found", fileName));
		}
	}

	private void save()
	{
		try
		{
			fileWorker.writeToFile(fileName, serializer.write(base));
			Log.v(TAG, String.format("Memory base \"%s\" saved", fileName));
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}
