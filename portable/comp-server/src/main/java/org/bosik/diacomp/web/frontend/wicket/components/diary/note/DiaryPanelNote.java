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
package org.bosik.diacomp.web.frontend.wicket.components.diary.note;

import java.util.TimeZone;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.merklesync.Versioned;

public class DiaryPanelNote extends Panel
{
	private static final long				serialVersionUID	= 1L;

	private IModel<Versioned<NoteRecord>>	model;

	public DiaryPanelNote(String id, Model<Versioned<NoteRecord>> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		NoteRecord rec = model.getObject().getData();
		TimeZone timeZone = MasterPage.getTimeZone(this);
		add(new Label("time", Utils.formatTimeLocalShort(timeZone, rec.getTime())));
		add(new Label("text", formatNoteText(rec.getText())));
	}

	private static String formatNoteText(String text)
	{
		// FIXME: remove tags
		return text;
	}
}
