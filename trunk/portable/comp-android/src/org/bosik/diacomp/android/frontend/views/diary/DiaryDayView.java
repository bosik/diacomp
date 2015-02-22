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
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
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
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.BaseAdapter;
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

	public static interface OnRecordClickListener
	{
		void onRecordClick(Versioned<? extends DiaryRecord> record);
	}

	public static interface OnHeaderClickListener
	{
		void onHeaderClick(Date date);
	}

	static final String		TAG				= DiaryDayView.class.getSimpleName();

	// Data
	Date					firstDate;
	int						countOfDays;
	List<Item>				data			= new ArrayList<Item>();
	boolean					loading			= false;
	boolean					loadingBefore	= false;
	boolean					loadingAfter	= false;
	int						offset;

	// Components
	BaseAdapter				adapter;
	public ListView			listRecs;

	// Services
	static DiaryService		diaryService;

	// Listeners
	OnRecordClickListener	onRecordClickListener;
	OnHeaderClickListener	onHeaderClickListener;

	public DiaryDayView(final Context context)
	{
		this(context, null);
	}

	int positionToIndex(int position)
	{
		return position - offset;
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

		setDate(c.getTime());

		// ================================================================

		adapter = new BaseAdapter()
		{
			@Override
			public int getViewTypeCount()
			{
				return 4;
			}

			@Override
			public int getItemViewType(int position)
			{
				final Object item = getItem(position);

				final int LOADING = -16;
				final int UNKNOWN = -17;

				if (item == null)
				{
					return LOADING;
				}
				else if (item instanceof ItemHeader)
				{
					return 1;
				}
				else if (item instanceof ItemData)
				{
					return 2;
				}
				else
				{
					return UNKNOWN;
				}
			}

			@Override
			public int getCount()
			{
				return Integer.MAX_VALUE / 2;
			}

			@Override
			public Object getItem(int position)
			{
				synchronized (data)
				{
					int index = positionToIndex(position);
					if (index >= 0 && index < data.size())
					{
						return data.get(index);
					}
					else
					{
						return null;
					}
				}
			}

			@Override
			public boolean hasStableIds()
			{
				return true;
			}

			@Override
			public long getItemId(int position)
			{
				final Object item = getItem(position);
				if (item != null)
				{
					return ((Item) item).extractDate().getTime();
				}
				else
				{
					return position;
				}
			}

			@SuppressWarnings("unchecked")
			@Override
			public View getView(int position, View convertView, ViewGroup parent)
			{
				Object item = getItem(position);

				if (item == null)
				{
					if (convertView == null)
					{
						convertView = inflater.inflate(R.layout.view_diary_rec_loading, null);
					}
				}
				else if (item instanceof ItemHeader)
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
					else if (data instanceof NoteRecord)
					{
						DiaryRecNoteView rec = new DiaryRecNoteView(context);
						rec.setData((Versioned<NoteRecord>) record);
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
		};
		offset = (Integer.MAX_VALUE / 2) / 2;
		listRecs.setAdapter(adapter);
		listRecs.setSelection(offset);

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
				int threshold = 10;
				if (positionToIndex(firstVisibleItem) < threshold)
				{
					loadBefore(4);
				}
				// else
				if (positionToIndex(firstVisibleItem + visibleItemCount) > data.size() - threshold)
				{
					loadAfter(4);
				}
			}
		});

		listRecs.setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id)
			{
				int index = positionToIndex(position);

				if (index < 0 || index >= data.size())
				{
					return;
				}

				Object item = data.get(index);

				if (item instanceof ItemHeader)
				{
					if (onHeaderClickListener != null)
					{
						onHeaderClickListener.onHeaderClick(((ItemHeader) item).date);
					}
				}
				else if (item instanceof ItemData)
				{
					Versioned<? extends DiaryRecord> record = ((ItemData) item).record;
					if (onRecordClickListener != null)
					{
						onRecordClickListener.onRecordClick(record);
					}
				}
				else
				{
					throw new RuntimeException("Invalid data type: " + item);
				}
			}
		});
	}

	public void setDate(Date date)
	{
		synchronized (data)
		{
			Log.d(TAG, "Refreshing: setting date = " + date);
			firstDate = date;
			countOfDays = 1;
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
		if (loading)
		{
			return;
		}

		final Date timeFrom = firstDate;
		final Date timeTo = Utils.shiftDate(firstDate, countOfDays);
		final int days = countOfDays;

		new AsyncTask<Date, Void, List<Item>>()
		{
			@Override
			protected void onPreExecute()
			{
				loading = true;
			}

			@Override
			protected List<Item> doInBackground(Date... params)
			{
				Date timeFrom = params[0];
				Date timeTo = params[1];
				Log.d(TAG, String.format("load(): %s - %s", timeFrom, timeTo));
				List<Versioned<? extends DiaryRecord>> records = request(timeFrom, timeTo);
				return groupItems(records, timeFrom, days);
			}

			@Override
			protected void onPostExecute(List<Item> items)
			{
				synchronized (data)
				{
					data = items;
				}
				adapter.notifyDataSetChanged();
				loading = false;
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

		new AsyncTask<Date, Void, List<Item>>()
		{
			@Override
			protected void onPreExecute()
			{
				loadingBefore = true;
			}

			@Override
			protected List<Item> doInBackground(Date... params)
			{
				Date timeFrom = params[0];
				Date timeTo = params[1];
				Log.d(TAG, String.format("loadBefore(): %s - %s", timeFrom, timeTo));
				final List<Versioned<? extends DiaryRecord>> records = request(timeFrom, timeTo);
				return groupItems(records, timeFrom, days);
			}

			@Override
			protected void onPostExecute(List<Item> items)
			{
				synchronized (data)
				{
					data.addAll(0, items);
				}
				int delta = items.size();
				Log.i(TAG, String.format("loadBefore(): %d new items loaded", delta));

				firstDate = timeFrom;
				countOfDays += days;

				loadingBefore = false;

				offset -= delta;
				adapter.notifyDataSetChanged();
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

		new AsyncTask<Date, Void, List<Item>>()
		{
			@Override
			protected void onPreExecute()
			{
				loadingAfter = true;
			}

			@Override
			protected List<Item> doInBackground(Date... params)
			{
				Date timeFrom = params[0];
				Date timeTo = params[1];
				Log.d(TAG, String.format("loadAfter(): %s and %s", timeFrom, timeTo));
				final List<Versioned<? extends DiaryRecord>> records = request(timeFrom, timeTo);
				return groupItems(records, timeFrom, days);
			}

			@Override
			protected void onPostExecute(List<Item> items)
			{
				synchronized (data)
				{
					data.addAll(items);
				}
				countOfDays += days;

				loadingAfter = false;
				adapter.notifyDataSetChanged();
			}
		}.execute(timeFrom, timeTo);
	}

	static List<Versioned<? extends DiaryRecord>> request(Date startTime, Date endTime)
	{
		List<Versioned<? extends DiaryRecord>> result = new ArrayList<Versioned<? extends DiaryRecord>>();
		result.addAll(diaryService.findPeriod(startTime, endTime, false));
		return result;
	}

	public void setOnHeaderClickListener(OnHeaderClickListener l)
	{
		onHeaderClickListener = l;
	}

	public void setOnRecordClickListener(OnRecordClickListener l)
	{
		onRecordClickListener = l;
	}
}