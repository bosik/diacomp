package org.bosik.diacomp.android.frontend.views.diary;

import java.util.Locale;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

public class DiaryRecInsView extends LinearLayout
{
	// Data
	private Versioned<InsRecord>	record;

	// Components
	private TextView				textTime;
	private TextView				textValue;

	public DiaryRecInsView(Context context)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_rec_ins, this);

		textTime = (TextView) findViewById(R.id.textInsTime);
		textValue = (TextView) findViewById(R.id.textInsValue);
	}

	public void setData(Versioned<InsRecord> record)
	{
		this.record = record;
		InsRecord data = record.getData();

		textTime.setText(Utils.formatTimeLocalShort(data.getTime()));

		String units = getContext().getString(R.string.common_unit_insulin);
		String text = String.format(Locale.US, "%.1f %s", data.getValue(), units);

		textValue.setText(text);
	}
}
