package org.bosik.diacomp.android.frontend.views.diary;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

public class DiaryRecNoteView extends LinearLayout
{
	// Data
	private Versioned<NoteRecord>	record;

	// Components
	private TextView				textTime;
	private TextView				textValue;

	public DiaryRecNoteView(Context context)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_rec_note, this);

		textTime = (TextView) findViewById(R.id.textNoteTime);
		textValue = (TextView) findViewById(R.id.textNoteValue);
	}

	public void setData(Versioned<NoteRecord> record)
	{
		this.record = record;
		NoteRecord data = record.getData();

		textTime.setText(Utils.formatTimeLocalShort(data.getTime()));
		textValue.setText(data.getText());
	}
}
