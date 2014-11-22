package org.bosik.diacomp.web.frontend.wicket.components.diary.page;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;

public class DiaryPanelDayModelObject implements Serializable
{
	private static final long				serialVersionUID	= 1L;

	private Date							date;
	private List<Versioned<DiaryRecord>>	items;

	public DiaryPanelDayModelObject(Date date, List<Versioned<DiaryRecord>> items)
	{
		this.date = date;
		this.items = items;
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
}
