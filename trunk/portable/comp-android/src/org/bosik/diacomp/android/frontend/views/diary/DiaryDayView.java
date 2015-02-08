package org.bosik.diacomp.android.frontend.views.diary;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.frontend.views.ExpandedListView;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.LinearLayout;
import android.widget.TextView;

public class DiaryDayView extends LinearLayout
{
	static final String						TAG		= DiaryDayView.class.getSimpleName();

	// Data
	private Date							date;
	List<Versioned<? extends DiaryRecord>>	recs	= new ArrayList<Versioned<? extends DiaryRecord>>();

	// Components
	private TextView						textDate;
	ExpandedListView						listRecs;

	// Services
	private static DiaryService				diaryService;

	public DiaryDayView(Context context)
	{
		this(context, new Date());
	}

	public DiaryDayView(Context context, Date date)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_day, this);

		Log.d(TAG, "DiaryDayView for date " + date);

		if (diaryService == null)
		{
			diaryService = new DiaryLocalService(getContext().getContentResolver());
			Log.i(TAG, "Diary service created");
		}

		textDate = (TextView) findViewById(R.id.textDayTitle);
		listRecs = (ExpandedListView) findViewById(R.id.listRecs);
		setDate(date);

		final BaseAdapter adapter = new BaseAdapter()
		{
			@Override
			public int getCount()
			{
				return recs.size();
			}

			@Override
			public Object getItem(int pos)
			{
				return pos;
			}

			@Override
			public long getItemId(int pos)
			{
				return pos;
			}

			@SuppressWarnings("unchecked")
			@Override
			public View getView(int pos, View v, ViewGroup p)
			{
				// View view =
				// LayoutInflater.from(FragmentBase.this.getActivity()).inflate(R.layout.view_iconed_line,
				// null);
				// return view;
				// DiaryDayView diaryDayView = new DiaryDayView(getActivity());
				// diaryDayView.setDate(new Date(System.currentTimeMillis() + Utils.MsecPerDay
				// * (pos - Integer.MAX_VALUE / 2)));
				// return diaryDayView;

				Versioned<? extends DiaryRecord> record = recs.get(pos);
				DiaryRecord data = record.getData();
				if (data instanceof BloodRecord)
				{
					DiaryRecBloodView rec = new DiaryRecBloodView(DiaryDayView.this.getContext());
					rec.setData((Versioned<BloodRecord>) record);
					return rec;
				}

				// temp stub for unknown record types
				DiaryRecBloodView rec = new DiaryRecBloodView(DiaryDayView.this.getContext());
				rec.setData(new Versioned<BloodRecord>(new BloodRecord()));
				return rec;

				// return null;
			}
		};
		listRecs.setAdapter(adapter);
		updateHeight();
		// listRecs.setOnFocusChangeListener(new OnFocusChangeListener()
		// {
		// @Override
		// public void onFocusChange(View v, boolean hasFocus)
		// {
		// updateContent();
		// }
		// });
	}

	void updateContent()
	{
		List<Versioned<DiaryRecord>> temp = diaryService.findPeriod(date, Utils.getNextDay(date), false);

		recs.clear();
		for (Versioned<DiaryRecord> item : temp)
		{
			if (item.getData() instanceof BloodRecord)
			{
				recs.add(item);
			}
		}

		int n = new Random().nextInt(10);

		// for (int i = 0; i < n; i++)
		// {
		// switch (new Random().nextInt(1))
		// {
		// case 0:
		// {
		// recs.add(new Versioned<BloodRecord>(new BloodRecord(new Date(), 5.0, 1)));
		// break;
		// }
		// case 1:
		// {
		// recs.add(new Versioned<InsRecord>(new InsRecord(new Date(), 12.0)));
		// break;
		// }
		// }
		// }

		updateHeight();
	}

	private void updateHeight()
	{
		ViewGroup.LayoutParams params = listRecs.getLayoutParams();
		params.height = listRecs.getFullHeight();
		Log.v(TAG, "Full height: " + params.height);
		listRecs.setLayoutParams(params);
	}

	public Date getDate()
	{
		return date;
	}

	public void setDate(Date date)
	{
		this.date = date;
		textDate.setText(Utils.formatDateLocal(date));
		updateContent();
	}
}
