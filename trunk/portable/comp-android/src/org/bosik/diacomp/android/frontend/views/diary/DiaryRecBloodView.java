package org.bosik.diacomp.android.frontend.views.diary;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

public class DiaryRecBloodView extends LinearLayout
{
	// Data
	private Versioned<BloodRecord>	data;

	// Components
	private TextView				textTime;

	public DiaryRecBloodView(Context context)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_rec_blood, this);

		textTime = (TextView) findViewById(R.id.textTime);
	}

	public void setData(Versioned<BloodRecord> record)
	{
		this.data = record;
		textTime.setText(Utils.formatTimeLocalShort(record.getData().getTime()));
	}
}
