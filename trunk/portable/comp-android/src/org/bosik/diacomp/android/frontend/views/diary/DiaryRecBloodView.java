package org.bosik.diacomp.android.frontend.views.diary;

import java.util.Locale;
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
	private Versioned<BloodRecord>	record;

	// Components
	private TextView				textTime;
	private TextView				textValue;

	private String[]				fingers	= !isInEditMode() ? getResources().getStringArray(R.array.fingers_short)
													: null;

	public DiaryRecBloodView(Context context)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_rec_blood, this);

		textTime = (TextView) findViewById(R.id.textBloodTime);
		textValue = (TextView) findViewById(R.id.textBloodValue);
	}

	public void setData(Versioned<BloodRecord> record)
	{
		this.record = record;
		BloodRecord data = record.getData();

		textTime.setText(Utils.formatTimeLocalShort(data.getTime()));

		String units = getContext().getString(R.string.common_bs_unit_mmol);
		String finger = data.getFinger() == -1 ? "" : String.format("(%s)", fingers[data.getFinger()]);
		String text = String.format(Locale.US, "%.1f %s %s", data.getValue(), units, finger);

		textValue.setText(text);
	}
}
