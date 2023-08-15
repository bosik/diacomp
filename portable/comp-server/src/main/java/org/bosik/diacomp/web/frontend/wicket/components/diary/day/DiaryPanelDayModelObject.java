/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.frontend.wicket.components.diary.day;

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.merklesync.Versioned;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

public class DiaryPanelDayModelObject implements Serializable
{
	private static final long				serialVersionUID	= 1L;

	private Date							date;
	private List<Versioned<DiaryRecord>>	items;
	private boolean							readOnly;

	public DiaryPanelDayModelObject(Date date, List<Versioned<DiaryRecord>> items, boolean readOnly)
	{
		this.date = date;
		this.items = items;
		this.readOnly = readOnly;
	}
	
	public DiaryPanelDayModelObject(Date date, List<Versioned<DiaryRecord>> items)
	{
		this(date, items, false);
	}

	public Date getDate()
	{
		return date;
	}

	public void setDate(Date date)
	{
		this.date = date;
	}

	public List<Versioned<DiaryRecord>> getItems()
	{
		return items;
	}

	public void setItems(List<Versioned<DiaryRecord>> items)
	{
		this.items = items;
	}

	public boolean isReadOnly()
	{
		return readOnly;
	}

	public void setReadOnly(boolean readOnly)
	{
		this.readOnly = readOnly;
	}
}
