package org.bosik.diacomp.android.backend.common;

/**
 * Local file base
 * 
 * @author Bosik
 * 
 * @param <T>
 *            Type of base's item
 */
public class FileBaseDAO
// <T extends Named>
// implements BaseService<T>
{
	// private static final String TAG = FileBaseDAO.class.getSimpleName();
	//
	// private MemoryBase<Versioned<T>> base;
	// private String fileName;
	// private Serializer<MemoryBase<T>> serializer;
	// private FileWorker fileWorker;

	// public FileBaseDAO(Context context, String fileName, Serializer<MemoryBase<T>> serializer)
	// throws IOException
	// {
	// this.fileName = fileName;
	// this.serializer = serializer;
	// fileWorker = new FileWorker(context);
	// load();
	// }

	// @Override
	// public String add(Versioned<T> item)
	// {
	// base.add(item);
	// save();
	// return item.getId();
	// }

	// @Override
	// public void delete(String id)
	// {
	// base.remove(id);
	// save();
	// }

	// @Override
	// public List<Versioned<T>> findAll()
	// {
	// return base.findAll();
	// }

	// @Override
	// public List<Versioned<T>> findAny(String filter)
	// {
	// return base.findAny(filter);
	// }

	// @Override
	// public Versioned<T> findById(String id)
	// {
	// return base.findById(id);
	// }
	//
	// @Override
	// public Versioned<T> findOne(String exactName)
	// {
	// return base.findOne(exactName);
	// }

	// @Override
	// public void update(Versioned<T> item) throws ItemNotFoundException
	// {
	// base.update(item);
	// save();
	// }

	// ----------------------------------- File I/O -----------------------------------

	// private void load() throws IOException
	// {
	// if (fileWorker.fileExists(fileName))
	// {
	// String source = fileWorker.readFromFile(fileName);
	// base = serializer.read(source);
	// Log.v(TAG, String.format("File base \"%s\" loaded, total items: %d", fileName,
	// base.count()));
	// }
	// else
	// {
	// base = new MemoryBase<Versioned<T>>();
	// Log.w(TAG, String.format("Failed to load file base \"%s\": file not found", fileName));
	// }
	// }

	// private void save() throws PersistenceException
	// {
	// try
	// {
	// fileWorker.writeToFile(fileName, serializer.write(base));
	// Log.v(TAG, String.format("File base \"%s\" saved", fileName));
	// }
	// catch (IOException e)
	// {
	// throw new PersistenceException(e);
	// }
	// }
}