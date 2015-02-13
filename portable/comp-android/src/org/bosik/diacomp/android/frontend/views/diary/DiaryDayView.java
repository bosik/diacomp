package org.bosik.diacomp.android.frontend.views.diary;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.frontend.views.ExpandedListView;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Context;
import android.os.AsyncTask;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

public class DiaryDayView extends LinearLayout
{
	public static interface OnLoadedListener
	{
		void onLoaded(int contentHeight);
	}

	static final String						TAG		= DiaryDayView.class.getSimpleName();

	// Data
	private Date							date;
	List<Versioned<? extends DiaryRecord>>	data	= new ArrayList<Versioned<? extends DiaryRecord>>();
	OnLoadedListener						onLoadedListener;

	// Components
	private TextView						textDate;
	ExpandedListView						listRecs;
	ProgressBar								progressBar;

	// Services
	static DiaryService						diaryService;

	public DiaryDayView(Context context)
	{
		// Should never be called actually
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
		progressBar = (ProgressBar) findViewById(R.id.diaryProgressBar);

		this.date = date;
		textDate.setText(Utils.formatDateLocal(date));

		// ================================================================

		final BaseAdapter adapter = new BaseAdapter()
		{
			@Override
			public int getCount()
			{
				return data.size();
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
				View result;

				Versioned<? extends DiaryRecord> record = data.get(pos);
				DiaryRecord data = record.getData();
				if (data instanceof BloodRecord)
				{
					DiaryRecBloodView rec = new DiaryRecBloodView(DiaryDayView.this.getContext());
					rec.setData((Versioned<BloodRecord>) record);
					result = rec;
				}
				else
				{
					// temp stub for unknown record types
					DiaryRecBloodView rec = new DiaryRecBloodView(DiaryDayView.this.getContext());
					rec.setData(new Versioned<BloodRecord>(new BloodRecord()));
					result = rec;
				}

				// Log.v(TAG, "pos: " + pos + "; first: " +
				// DiaryDayView.this.parentList.getFirstVisiblePosition());

				// if (pos <= DiaryDayView.this.parentList.getFirstVisiblePosition())
				// {
				// result.measure(ViewGroup.LayoutParams.MATCH_PARENT,
				// ViewGroup.LayoutParams.WRAP_CONTENT);
				// int itemHeight = result.getMeasuredHeight();
				// DiaryDayView.this.parentList.scrollBy(0, itemHeight);
				// Log.v(TAG, "Scrolling to " + -itemHeight);
				// }

				return result;
			}
		};
		listRecs.setAdapter(adapter);
		updateContent();

		// ============================================================================

		// listRecs.setOnFocusChangeListener(new OnFocusChangeListener()
		// {
		// @Override
		// public void onFocusChange(View v, boolean hasFocus)
		// {
		// updateContent();
		// }
		// });
	}

	private void updateContent()
	{
		new AsyncTask<Date, Void, List<Versioned<? extends DiaryRecord>>>()
		{
			@Override
			protected void onPreExecute()
			{
				progressBar.setVisibility(View.VISIBLE);
			}

			@Override
			protected List<Versioned<? extends DiaryRecord>> doInBackground(Date... params)
			{
				return request(params[0]);
			}

			@Override
			protected void onPostExecute(List<Versioned<? extends DiaryRecord>> result)
			{
				progressBar.setVisibility(View.GONE);
				data = result;

				((BaseAdapter) listRecs.getAdapter()).notifyDataSetChanged();

				int contentHeight = updateHeight();
				// if (onLoadedListener != null)
				// {
				// onLoadedListener.onLoaded(contentHeight);
				// }
			}
		}.execute(date);

		// int contentHeight = updateHeight();
		// if (onLoadedListener != null)
		// {
		// onLoadedListener.onLoaded(contentHeight);
		// }
	}

	int updateHeight()
	{
		// emulating height="wrap_content"
		ViewGroup.LayoutParams params = listRecs.getLayoutParams();
		params.height = listRecs.getFullHeight();
		listRecs.setLayoutParams(params);
		Log.v(TAG, "Full height: " + params.height);
		return params.height;
	}

	List<Versioned<? extends DiaryRecord>> request(Date date)
	{
		Date startTime = date;
		Date endTime = Utils.getNextDay(date);
		List<Versioned<DiaryRecord>> temp = diaryService.findPeriod(startTime, endTime, false);

		List<Versioned<? extends DiaryRecord>> result = new ArrayList<Versioned<? extends DiaryRecord>>();

		for (Versioned<DiaryRecord> item : temp)
		{
			if (item.getData() instanceof BloodRecord)
			{
				result.add(item);
			}
		}

		return result;
	}

	public Date getDate()
	{
		return date;
	}

	public void setDate(Date date)
	{
		this.date = date;
		updateContent();
	}

	public void setOnLoadedListener(OnLoadedListener onLoadedListener)
	{
		this.onLoadedListener = onLoadedListener;
	}
}
