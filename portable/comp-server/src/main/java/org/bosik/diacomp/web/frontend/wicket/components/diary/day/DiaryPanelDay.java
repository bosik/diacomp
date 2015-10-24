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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.wicket.components.diary.blood.DiaryPanelBlood;
import org.bosik.diacomp.web.frontend.wicket.components.diary.ins.DiaryPanelIns;
import org.bosik.diacomp.web.frontend.wicket.components.diary.meal.DiaryPanelMeal;
import org.bosik.diacomp.web.frontend.wicket.components.diary.note.DiaryPanelNote;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.merklesync.Versioned;

public class DiaryPanelDay extends Panel
{
	private static final long			serialVersionUID	= 1L;

	IModel<DiaryPanelDayModelObject>	model;

	public DiaryPanelDay(String id, IModel<DiaryPanelDayModelObject> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final DiaryPanelDayModelObject data = model.getObject();

		TimeZone timeZone = MasterPage.getTimeZone(this);
		add(new Label("caption", Utils.formatDateLocal(timeZone, data.getDate())));
		add(new RefreshingView<Versioned<DiaryRecord>>("diaryRecord")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected Iterator<IModel<Versioned<DiaryRecord>>> getItemModels()
			{
				final List<IModel<Versioned<DiaryRecord>>> list = new ArrayList<IModel<Versioned<DiaryRecord>>>();

				for (Versioned<DiaryRecord> item : data.getItems())
				{
					list.add(Model.<Versioned<DiaryRecord>> of(item));
				}

				return list.iterator();
			}

			@Override
			protected void populateItem(final Item<Versioned<DiaryRecord>> item)
			{
				Versioned<DiaryRecord> record = item.getModelObject();
				DiaryRecord data = record.getData();

				boolean readOnly = DiaryPanelDay.this.model.getObject().isReadOnly();
				
				if (data instanceof BloodRecord)
				{
					item.add(new DiaryPanelBlood("diaryRecordPanel", Model.of(new Versioned<BloodRecord>(record))));
				}
				else if (data instanceof InsRecord)
				{
					item.add(new DiaryPanelIns("diaryRecordPanel", Model.of(new Versioned<InsRecord>(record))));
				}
				else if (data instanceof MealRecord)
				{
					item.add(new DiaryPanelMeal("diaryRecordPanel", Model.of(new Versioned<MealRecord>(record)), readOnly));
				}
				else if (data instanceof NoteRecord)
				{
					item.add(new DiaryPanelNote("diaryRecordPanel", Model.of(new Versioned<NoteRecord>(record))));
				}
				else
				{
					// ignore unknown types
				}
			}
		});
	}
}
