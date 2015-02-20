package org.bosik.diacomp.android.frontend.views.diary;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Context;
import android.os.AsyncTask;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AbsListView.OnScrollListener;
import android.widget.BaseAdapter;
import android.widget.HeaderViewListAdapter;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;

public class DiaryDayView extends LinearLayout
{
	private static abstract class Item
	{
		public abstract Date extractDate();
	}

	private static class ItemHeader extends Item
	{
		public Date	date;

		public ItemHeader(Date date)
		{
			this.date = date;
		}

		@Override
		public Date extractDate()
		{
			return date;
		}
	}

	private static class ItemData extends Item
	{
		public Versioned<? extends DiaryRecord>	record;

		public ItemData(Versioned<? extends DiaryRecord> record)
		{
			this.record = record;
		}

		@Override
		public Date extractDate()
		{
			return record.getData().getTime();
		}
	}

	static final String	TAG				= DiaryDayView.class.getSimpleName();

	// Data
	Date				firstDate;
	int					countOfDays;
	List<Item>			data			= new ArrayList<Item>();
	boolean				loading			= false;
	boolean				loadingBefore	= false;
	boolean				loadingAfter	= false;

	// Components
	public ListView		listRecs;

	// Services
	static DiaryService	diaryService;

	public DiaryDayView(final Context context)
	{
		this(context, null);
	}

	public DiaryDayView(final Context context, AttributeSet attributes)
	{
		super(context, attributes);
		final LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_day, this);

		if (diaryService == null)
		{
			diaryService = new DiaryLocalService(getContext().getContentResolver());
			Log.i(TAG, "Diary service created");
		}

		listRecs = (ListView) findViewById(R.id.listRecs);

		Calendar c = Calendar.getInstance();
		c.set(Calendar.HOUR_OF_DAY, 0);
		c.set(Calendar.MINUTE, 0);
		c.set(Calendar.SECOND, 0);
		c.set(Calendar.MILLISECOND, 0);

		setDate(Utils.getNextDay(c.getTime()));

		// ================================================================

		View progressView = inflater.inflate(R.layout.view_diary_rec_loading, null);
		listRecs.addHeaderView(progressView);
		listRecs.addFooterView(progressView);

		final BaseAdapter adapter = new BaseAdapter()
		{
			@Override
			public int getViewTypeCount()
			{
				return 3;
			}

			@Override
			public int getItemViewType(int position)
			{
				if (data.get(position) instanceof ItemHeader)
				{
					return 1;
				}
				else if (data.get(position) instanceof ItemData)
				{
					return 2;
				}
				else
				{
					return -17;
				}
			}

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
			public View getView(int position, View convertView, ViewGroup parent)
			{
				synchronized (data)
				{
					int index = position;
					Log.d(TAG,
							String.format("getView() for position %d / FV: %d", position,
									listRecs.getFirstVisiblePosition()));

					Item item = data.get(index);

					if (item instanceof ItemHeader)
					{
						if (convertView == null)
						{
							LayoutInflater inflater = (LayoutInflater) context
									.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
							convertView = inflater.inflate(R.layout.view_diary_rec_header, null);
						}

						TextView textTitle = (TextView) convertView.findViewById(R.id.diaryDayHeader);
						textTitle.setText(Utils.formatDateLocal(((ItemHeader) item).date));
					}
					else if (item instanceof ItemData)
					{
						Versioned<? extends DiaryRecord> record = ((ItemData) item).record;
						DiaryRecord data = record.getData();
						if (data instanceof BloodRecord)
						{
							DiaryRecBloodView rec = new DiaryRecBloodView(context);
							rec.setData((Versioned<BloodRecord>) record);
							convertView = rec;
						}
						else if (data instanceof InsRecord)
						{
							DiaryRecInsView rec = new DiaryRecInsView(context);
							rec.setData((Versioned<InsRecord>) record);
							convertView = rec;
						}
						else if (data instanceof MealRecord)
						{
							DiaryRecMealView rec = new DiaryRecMealView(context);
							rec.setData((Versioned<MealRecord>) record);
							convertView = rec;
						}
						else
						{
							throw new RuntimeException("Unsupported data type: " + data.getClass().getSimpleName());
						}
					}
					else
					{
						throw new RuntimeException("Invalid data type: " + item);
					}

					return convertView;
				}
			}
		};
		listRecs.setAdapter(adapter);
		listRecs.setSelection(1);

		// ============================================================================

		listRecs.setOnScrollListener(new OnScrollListener()
		{
			@Override
			public void onScrollStateChanged(AbsListView view, int scrollState)
			{
			}

			@Override
			public void onScroll(AbsListView view, int firstVisibleItem, int visibleItemCount, int totalItemCount)
			{
				int threshold = 2;
				if (firstVisibleItem < threshold)
				{
					loadBefore(1);
				}
				else if (firstVisibleItem + visibleItemCount > totalItemCount - threshold)
				{
					loadAfter(1);
				}
			}
		});
	}

	public void setDate(Date date)
	{
		synchronized (data)
		{
			Log.d(TAG, "Refreshing: setting date = " + date);
			firstDate = Utils.getPrevDay(date);
			countOfDays = 3;
			refresh();
		}
	}

	List<Item> groupItems(List<Versioned<? extends DiaryRecord>> records, Date firstDate, int countOfDays)
	{
		List<Item> result = new ArrayList<Item>();

		Date curDate = firstDate;
		int index = 0;
		for (int i = 1; i <= countOfDays; i++, curDate = Utils.getNextDay(curDate))
		{
			// TODO: it's time, not the date
			result.add(new ItemHeader(curDate));
			while (index < records.size() && sameDate(curDate, records.get(index).getData().getTime()))
			{
				result.add(new ItemData(records.get(index)));
				index++;
			}
		}

		return result;
	}

	public void refresh()
	{
		// int index = listRecs.getFirstVisiblePosition();
		// Log.d(TAG, "Refreshing: first visible index = " + index);
		// if (!data.isEmpty())
		// {
		// Log.d(TAG, "Refreshing: data is not empty");
		// setDate(data.get(index).extractDate());
		// }
		// else
		// {
		// Calendar c = Calendar.getInstance();
		// c.set(Calendar.HOUR_OF_DAY, 0);
		// c.set(Calendar.MINUTE, 0);
		// c.set(Calendar.SECOND, 0);
		// c.set(Calendar.MILLISECOND, 0);
		// setDate(c.getTime());
		// }

		if (loading)
		{
			return;
		}

		final Date timeFrom = firstDate;
		final Date timeTo = Utils.shiftDate(firstDate, countOfDays);
		final int days = countOfDays;

		new AsyncTask<Date, Void, List<Versioned<? extends DiaryRecord>>>()
		{
			@Override
			protected void onPreExecute()
			{
				loading = true;
			}

			@Override
			protected List<Versioned<? extends DiaryRecord>> doInBackground(Date... params)
			{
				Date timeFrom = params[0];
				Date timeTo = params[1];
				Log.d(TAG, String.format("load(): %s - %s", timeFrom, timeTo));
				return request(timeFrom, timeTo);
			}

			@Override
			protected void onPostExecute(List<Versioned<? extends DiaryRecord>> records)
			{
				synchronized (data)
				{
					data = groupItems(records, timeFrom, days);

					final int oldFirst = listRecs.getFirstVisiblePosition();
					int c = listRecs.getChildAt(0).getTop();

					((BaseAdapter) ((HeaderViewListAdapter) listRecs.getAdapter()).getWrappedAdapter())
							.notifyDataSetChanged();
					listRecs.setSelection(oldFirst);
					// listRecs.smoothScrollToPositionFromTop(oldFirst + delta + 1, 0);

					listRecs.scrollBy(0, -c);

					loading = false;
				}
			}
		}.execute(timeFrom, timeTo);
	}

	// TODO: optimize and move to Utils
	static boolean sameDate(Date date1, Date date2)
	{
		return Utils.formatDateLocal(date1).equals(Utils.formatDateLocal(date2));
	}

	void loadBefore(final int days)
	{
		if (loadingBefore)
		{
			Log.w(TAG, "loadBefore() ignored");
			return;
		}

		final Date timeFrom = Utils.shiftDate(firstDate, -days);
		final Date timeTo = firstDate;

		new AsyncTask<Date, Void, List<Versioned<? extends DiaryRecord>>>()
		{
			@Override
			protected void onPreExecute()
			{
				loadingBefore = true;
			}

			@Override
			protected List<Versioned<? extends DiaryRecord>> doInBackground(Date... params)
			{
				Date timeFrom = params[0];
				Date timeTo = params[1];
				Log.d(TAG, String.format("loadBefore(): %s - %s", timeFrom, timeTo));
				return request(timeFrom, timeTo);
			}

			@Override
			protected void onPostExecute(List<Versioned<? extends DiaryRecord>> records)
			{
				synchronized (data)
				{
					List<Item> newItems = groupItems(records, timeFrom, days);
					data.addAll(0, newItems);

					int delta = newItems.size();
					Log.i(TAG, String.format("loadBefore(): %d new items loaded", delta));

					int oldFirst = listRecs.getFirstVisiblePosition();
					int c = listRecs.getChildAt(0).getTop();

					firstDate = timeFrom;
					countOfDays += days;

					loadingBefore = false;

					((BaseAdapter) ((HeaderViewListAdapter) listRecs.getAdapter()).getWrappedAdapter())
							.notifyDataSetChanged();
					listRecs.setSelection(oldFirst + delta);
					listRecs.scrollBy(0, -c);

					// listRecs.smoothScrollToPositionFromTop(oldFirst + delta + 1, 0);
				}
			}
		}.execute(timeFrom, timeTo);
	}

	void loadAfter(final int days)
	{
		if (loadingAfter)
		{
			Log.w(TAG, "loadAfter() ignored");
			return;
		}

		final Date timeFrom = Utils.shiftDate(firstDate, countOfDays);
		final Date timeTo = Utils.shiftDate(firstDate, countOfDays + days);

		new AsyncTask<Date, Void, List<Versioned<? extends DiaryRecord>>>()
		{
			@Override
			protected void onPreExecute()
			{
				loadingAfter = true;
			}

			@Override
			protected List<Versioned<? extends DiaryRecord>> doInBackground(Date... params)
			{
				Date timeFrom = params[0];
				Date timeTo = params[1];
				Log.d(TAG, String.format("loadAfter(): %s and %s", timeFrom, timeTo));
				return request(timeFrom, timeTo);
			}

			@Override
			protected void onPostExecute(List<Versioned<? extends DiaryRecord>> records)
			{
				synchronized (data)
				{
					List<Item> newItems = groupItems(records, timeFrom, days);
					data.addAll(newItems);
					countOfDays += days;

					loadingAfter = false;
					((BaseAdapter) ((HeaderViewListAdapter) listRecs.getAdapter()).getWrappedAdapter())
							.notifyDataSetChanged();
				}
			}
		}.execute(timeFrom, timeTo);
	}

	static List<Versioned<? extends DiaryRecord>> request(Date startTime, Date endTime)
	{
		List<Versioned<DiaryRecord>> temp = diaryService.findPeriod(startTime, endTime, false);

		List<Versioned<? extends DiaryRecord>> result = new ArrayList<Versioned<? extends DiaryRecord>>();

		for (Versioned<DiaryRecord> item : temp)
		{
			if (item.getData() instanceof BloodRecord || item.getData() instanceof InsRecord
					|| item.getData() instanceof MealRecord)
			{
				result.add(item);
			}
		}

		return result;
	}
}