package org.bosik.diacomp.web.frontend.wicket.components.diary.ins;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.image.Image;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.utils.Utils;

public class DiaryPanelIns extends Panel
{
	private static final long	serialVersionUID	= 1L;

	public DiaryPanelIns(String id, InsRecord rec)
	{
		super(id);
		add(new Image("icon", new Model<String>("icon.png")).add(AttributeModifier.replace("title", "Инъекция")));
		add(new Label("time", Utils.formatTimeLocalShort(rec.getTime())));
		add(new Label("value", formatInsValue(rec.getValue())));
	}

	private static String formatInsValue(double value)
	{
		return Utils.formatDoubleShort(value) + " ед";
	}
}