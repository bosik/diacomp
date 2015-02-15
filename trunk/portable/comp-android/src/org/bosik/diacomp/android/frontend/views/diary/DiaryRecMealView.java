package org.bosik.diacomp.android.frontend.views.diary;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.views.diary.MealFormatter.FormatStyle;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

public class DiaryRecMealView extends LinearLayout
{
	// Data
	private Versioned<MealRecord>	record;

	// Components
	private TextView				textTime;
	private TextView				textValue;

	public DiaryRecMealView(Context context)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_rec_meal, this);

		textTime = (TextView) findViewById(R.id.textMealTime);
		textValue = (TextView) findViewById(R.id.textMealValue);
	}

	public void setData(Versioned<MealRecord> record)
	{
		this.record = record;
		MealRecord data = record.getData();

		textTime.setText(Utils.formatTimeLocalShort(data.getTime()));
		textValue.setText(MealFormatter.format(record.getData(), FormatStyle.MOST_CARBS));
	}
}
