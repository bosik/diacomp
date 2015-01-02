package org.bosik.diacomp.web.frontend.wicket.components.diary.note;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.utils.Utils;

public class DiaryPanelNote extends Panel
{
	private static final long	serialVersionUID	= 1L;

	private IModel<NoteRecord>	model;

	public DiaryPanelNote(String id, IModel<NoteRecord> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		NoteRecord rec = model.getObject();
		add(new Label("time", Utils.formatTimeLocalShort(rec.getTime())));
		add(new Label("text", formatNoteText(rec.getText())));
	}

	private static String formatNoteText(String text)
	{
		// FIXME: remove tags
		return text;
	}
}
