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
package org.bosik.diacomp.web.frontend.wicket.components.diary.ins;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.image.Image;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.merklesync.Versioned;

import java.util.TimeZone;

public class DiaryPanelIns extends Panel
{
	private static final long				serialVersionUID	= 1L;

	private IModel<Versioned<InsRecord>>	model;

	public DiaryPanelIns(String id, Model<Versioned<InsRecord>> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		InsRecord rec = model.getObject().getData();
		// TODO: localization
		add(new Image("icon", Model.of("icon.png")).add(AttributeModifier.replace("title", "Инъекция")));
		TimeZone timeZone = MasterPage.getTimeZone(this);
		add(new Label("time", Utils.formatTimeLocalShort(timeZone, rec.getTime())));
		add(new Label("value", formatInsValue(rec.getValue())));
	}

	private static String formatInsValue(double value)
	{
		return Utils.formatDoubleShort(value) + " ед";
	}
}
