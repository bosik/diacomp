package org.bosik.compensation.persistence.common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Хранящаяся в памяти база
 * 
 * @author Bosik
 * 
 * @param <T>
 *            Тип элемента базы
 */
public class MemoryBase<T extends UniqueNamed>
{
	private final List<T>	items		= new ArrayList<T>();
	private int				version		= 0;
	private boolean			silentMode	= false;

	// ================== РАБОТА СО СПИСКОМ ==================

	public int add(T item)
	{
		if (null == item)
		{
			throw new NullPointerException("Item can't be null");
		}

		try
		{
			items.add((T) item.clone());
			changed();
			return items.indexOf(item);
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

	public T get(String id)
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

	public List<T> getAll()
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

	public void remove(int index)
	{
		items.remove(index);
		changed();
	}

	public void remove(T item)
	{
		items.remove(item);
		changed();
	}

	public boolean remove(String id)
	{
		for (Iterator<T> iterator = items.iterator(); iterator.hasNext();)
		{
			T item = iterator.next();
			if (item.getId().equals(id))
			{
				iterator.remove();
				changed();
				return true;
			}
		}
		return false;
	}

	public void update(T item)
	{
		// comparison performed by id, because equals() method is overridden
		try
		{
			int index = items.indexOf(item);
			items.set(index, (T) item.clone());
			changed();
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

	// ================== КОНТРОЛЬ НОМЕРА ВЕРСИИ ==================

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
