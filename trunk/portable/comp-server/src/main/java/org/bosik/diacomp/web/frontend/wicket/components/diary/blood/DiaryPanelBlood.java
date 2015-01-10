package org.bosik.diacomp.web.frontend.wicket.components.diary.blood;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.image.Image;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.utils.Utils;

public class DiaryPanelBlood extends Panel
{
	private static final long				serialVersionUID	= 1L;

	private IModel<BloodRecord>	model;

	public DiaryPanelBlood(String id, IModel<BloodRecord> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		BloodRecord rec = model.getObject();
		// TODO: localization
		add(new Image("icon", Model.of("icon.png")).add(AttributeModifier.replace("title", "Замер СК")));
		add(new Label("time", Utils.formatTimeLocalShort(rec.getTime())));
		add(new Label("value", formatBloodValue(rec.getValue())));
		add(new Label("finger", formatBloodFinger(rec.getFinger())).add(AttributeModifier.replace("title",
				formatBloodFingerHint(rec.getFinger()))));
	}

	private static String formatBloodValue(double value)
	{
		return String.format("%.1f ", value) + "ммоль/л";
	}

	private static String formatBloodFinger(int index)
	{
		String[] fingers = { "БЛ", "1Л", "2Л", "3Л", "4Л", "4П", "3П", "2П", "1П", "БП" };
		return fingers[index];
	}

	private static String formatBloodFingerHint(int index)
	{
		String[] fingers = { "Левая, большой", "Левая, указательный", "Левая, средний",
				"Левая, безымянный", "Левая, мизинец", "Правая, мизинец", "Правая, безымянный", "Правая, средний",
				"Правая, указательный", "Правая, большой" };
		return fingers[index];
	}
}
