package org.bosik.compensation.persistence.entity.common;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.utils.Utils;

public class CustomBase<T extends CustomItem>
{
	private static int idCounter = 0;

	private final List<T> items = new ArrayList<T>();
	private Date timeStamp = null;
	private int version = 0;

	private boolean silentMode = false;

	// ================== РАБОТА СО СПИСКОМ ==================

	public int add(T item)
	{
		if (null == item)
			throw new NullPointerException("Item can't be null");

		try
		{
			item.setId(++idCounter);	
			items.add((T) item.clone());
			changed();
			return items.indexOf(item);
		} catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public T get(int index)
	{
		try
		{
			return (T) items.get(index).clone();
		} catch (CloneNotSupportedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public void update(T item)
	{
		// comparison performed by id, because equals() method is overridden
		try
		{
			int index = items.indexOf(item);
			items.set(index, (T) item.clone());
			changed();
		} catch (CloneNotSupportedException e)
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

	public int count()
	{
		return items.size();
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

	public Date getTimeStamp()
	{
		return timeStamp;
	}

	public void setTimeStamp(Date timeStamp)
	{
		this.timeStamp = timeStamp;
	}

	// ================== ТИХИЙ РЕЖИМ ==================

	public void beginUpdate()
	{
		silentMode = true;
	}

	public void endUpdate()
	{
		silentMode = false;
	}

	// ================== СЛУЖЕБНЫЕ ==================
	
	private void changed()
	{
		if (!silentMode)
		{
			timeStamp = Utils.now();
			version++;
		}
	}
}
