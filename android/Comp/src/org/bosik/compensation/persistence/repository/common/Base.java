package org.bosik.compensation.persistence.repository.common;

import java.util.ArrayList;
import java.util.List;
import org.bosik.compensation.persistence.entity.common.Item;

/**
 * Хранящаяся в памяти база
 * 
 * @author Bosik
 * 
 * @param <T>
 *            Тип элемента базы
 */
public class Base<T extends Item>
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
