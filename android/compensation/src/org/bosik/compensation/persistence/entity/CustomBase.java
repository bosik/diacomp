package org.bosik.compensation.persistence.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.utils.Utils;

public class CustomBase<T extends CustomItem> implements ChangeListener
{
	private final List<T> items = new ArrayList<T>();
	private Date timeStamp = null;
	private int version = 0;

	// ================== РАБОТА СО СПИСКОМ ==================

	public void add(T item)
	{
		if (null == item)
			throw new NullPointerException("Item can't be null");

		item.setChangeListener(this);
		items.add(item);
	}

	public void remove(int index)
	{
		items.remove(index);
	}

	public T get(int index)
	{
		return items.get(index);
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

	// ================== ОБРАБОТКА СООБЩЕНИЙ ОТ ЭЛЕМЕНТОВ СПИСКА ==================

	@Override
	public void changed()
	{
		timeStamp = Utils.now();
		version++;
	}
}
