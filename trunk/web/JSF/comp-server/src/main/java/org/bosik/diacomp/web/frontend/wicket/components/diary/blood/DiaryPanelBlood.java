package org.bosik.diacomp.web.frontend.wicket.components.diary.blood;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.utils.Utils;

public class DiaryPanelBlood extends Panel
{
	private static final long	serialVersionUID	= 1L;

	//private String[] fingerShort = new String[10];

	public DiaryPanelBlood(String id, BloodRecord rec)
	{
		super(id);

		add(new Label("time", Utils.formatTimeLocalShort(rec.getTime())));
		add(new Label("value", formatBloodValue(rec.getValue())));
		add(new Label("finger", formatBloodFinger(rec.getFinger())).add(AttributeModifier.replace("title",
				formatBloodFingerHint(rec.getFinger()))));

		//		add(new ExternalLink("linkLogout", "j_spring_security_logout").setVisible(false));
		//		add(new BookmarkablePageLink<Void>("linkHome", AboutPage.class));
		//		add(new Label("infoLogin"));
	}

	private static String formatBloodValue(double value)
	{
		return String.format("%.1f", value) + " ммоль/л";
	}

	private static String formatBloodFinger(int index)
	{
		String[] fingers = new String[] { "БЛ", "1Л", "2Л", "3Л", "4Л", "4П", "3П", "2П", "1П", "БП" };
		return fingers[index];
	}

	private static String formatBloodFingerHint(int index)
	{
		String[] fingers = new String[] { "Левая, большой", "Левая, указательный", "Левая, средний",
				"Левая, безымянный", "Левая, мизинец", "Правая, мизинец", "Правая, безымянный", "Правая, средний",
				"Правая, указательный", "Правая, большой" };
		return fingers[index];
	}
}
