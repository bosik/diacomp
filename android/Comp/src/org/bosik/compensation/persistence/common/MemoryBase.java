package org.bosik.compensation.persistence.common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import org.bosik.compensation.bo.basic.UniqueNamed;
import org.bosik.compensation.persistence.exceptions.DuplicateException;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;

/**
 * In-memory base
 * 
 * @author Bosik
 * 
 * @param <T>
 *            Base item's type
 */
@SuppressWarnings("unchecked")
public class MemoryBase<T extends UniqueNamed>
{
	private final List<T>	items		= new ArrayList<T>();
	private int				version		= 0;
	private boolean			silentMode	= false;

	// ================== LIST ROUTINES ==================

	public int add(T item) throws DuplicateException
	{
		if (null == item)
		{
			throw new NullPointerException("Item can't be null");
		}

		try
		{
			if (findById(item.getId()) == null)
			{
				items.add((T) item.clone());
				changed();
				return items.size() - 1;
			}
			else
			{
				throw new DuplicateException(item.getId());
			}
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public void clear()
	{
		items.clear();
		changed();
	}

	public int count()
	{
		return items.size();
	}

	public T get(int index)
	{
		try
		{
			return (T) items.get(index).clone();
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public T findById(String id)
	{
		try
		{
			for (T item : items)
			{
				if (item.getId().equals(id))
				{
					return (T) item.clone();
				}
			}
			return null;
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public List<T> findAll()
	{
		try
		{
			List<T> result = new ArrayList<T>();
			for (T item : items)
			{
				result.add((T) item.clone());
			}
			return result;
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public List<T> findAny(String filter)
	{
		try
		{
			List<T> result = new ArrayList<T>();
			filter = filter.toUpperCase(Locale.US);

			for (T item : items)
			{
				if (item.getName().toUpperCase(Locale.US).contains(filter))
				{
					result.add((T) item.clone());
				}
			}

			return result;
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public T findOne(String exactName)
	{
		try
		{
			for (T item : items)
			{
				if (item.getName().equals(exactName))
				{
					return (T) item.clone();
				}
			}
			return null;
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public void remove(String id) throws ItemNotFoundException
	{
		for (Iterator<T> iterator = items.iterator(); iterator.hasNext();)
		{
			T item = iterator.next();
			if (item.getId().equals(id))
			{
				iterator.remove();
				changed();
				return;
			}
		}
		throw new ItemNotFoundException(id);
	}

	public void replaceAll(List<T> newList, int newVersion)
	{
		try
		{
			items.clear();
			for (T item : newList)
			{
				items.add((T) item.clone());
			}
			setVersion(newVersion);
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public void update(T item) throws ItemNotFoundException
	{
		// comparison performed by id, because equals() method is overridden
		try
		{
			int index = items.indexOf(item);
			if (index > -1)
			{
				items.set(index, (T) item.clone());
				changed();
			}
			else
			{
				throw new ItemNotFoundException(item.getId());
			}
		}
		catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	// ================== GET / SET ==================

	public int getVersion()
	{
		return version;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	// ================== VERSION CONTROL ==================

	public void beginUpdate()
	{
		silentMode = true;
	}

	public void endUpdate()
	{
		silentMode = false;
	}

	private void changed()
	{
		if (!silentMode)
		{
			version++;
		}
	}
}
