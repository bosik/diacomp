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
	private Date		date;
	List<Item>			data			= new ArrayList<Item>();
	boolean				loadingBefore	= false;
	boolean				loadingAfter	= false;

	// Components
	ListView			listRecs;

	// Services
	static DiaryService	diaryService;

	public DiaryDayView(final Context context)
	{
		this(context, null);
	}

	public DiaryDayView(final Context context, AttributeSet attributes)
	{
		super(context);
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

		// FIXME: DEBUG ONLY
		c.set(Calendar.DAY_OF_YEAR, 1);

		setDate(c.getTime());

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
				int threshold = 15;
				if (firstVisibleItem < threshold)
				{
					loadBefore(getFirstDate());
				}
				else if (firstVisibleItem + visibleItemCount > totalItemCount - threshold)
				{
					loadAfter(getLastDate());
				}
			}
		});
	}

	public void setDate(Date date)
	{
		synchronized (data)
		{
			// TODO: workaround
			this.date = Utils.getNextDay(date);
			data.clear();
			loadAfter(date);
		}
	}

	Date getFirstDate()
	{
		synchronized (data)
		{
			if (!data.isEmpty())
			{
				Item item = data.get(0);
				return item.extractDate();
			}
			else
			{
				return date;
			}
		}
	}

	Date getLastDate()
	{
		synchronized (data)
		{
			Date result;
			if (!data.isEmpty())
			{
				Item item = data.get(data.size() - 1);
				result = item.extractDate();
			}
			else
			{
				result = date;
			}

			// to exclude the last date
			return new Date(result.getTime() + 1000);
		}
	}

	// TODO: optimize and move to Utils
	static boolean sameDate(Date date1, Date date2)
	{
		return Utils.formatDateLocal(date1).equals(Utils.formatDateLocal(date2));
	}

	void loadBefore(Date date)
	{
		if (loadingBefore)
		{
			Log.w(TAG, "loadBefore() ignored");
			return;
		}

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
				Date timeTo = params[0];
				Date timeFrom = Utils.getPrevDay(timeTo);
				Log.d(TAG, String.format("loadBefore(): %s and %s", timeFrom, timeTo));
				return request(timeFrom, timeTo);
			}

			@Override
			protected void onPostExecute(List<Versioned<? extends DiaryRecord>> records)
			{
				synchronized (data)
				{
					Date nextDate = getFirstDate();
					int oldSize = data.size();

					if (!records.isEmpty())
					{
						for (int i = records.size() - 1; i >= 0; i--)
						{
							Versioned<? extends DiaryRecord> record = records.get(i);
							if (!sameDate(record.getData().getTime(), nextDate))
							{
								// TODO: it's time, not the date
								data.add(0, new ItemHeader(nextDate));
							}
							data.add(0, new ItemData(record));
							nextDate = record.getData().getTime();
						}
					}
					else
					{
						data.add(0, new ItemHeader(Utils.getPrevDay(getFirstDate())));
					}

					final int delta = data.size() - oldSize;

					Log.i(TAG, String.format("loadBefore(): %d new items loaded", delta));

					final int oldFirst = listRecs.getFirstVisiblePosition();
					((BaseAdapter) ((HeaderViewListAdapter) listRecs.getAdapter()).getWrappedAdapter())
							.notifyDataSetChanged();
					listRecs.setSelection(oldFirst + delta + 1);

					loadingBefore = false;
				}
			}
		}.execute(date);
	}

	void loadAfter(Date date)
	{
		if (loadingAfter)
		{
			Log.w(TAG, "loadAfter() ignored");
			return;
		}

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
				Date timeTo = Utils.getNextDay(timeFrom);
				Log.d(TAG, String.format("loadAfter(): %s and %s", timeFrom, timeTo));
				return request(timeFrom, timeTo);
			}

			@Override
			protected void onPostExecute(List<Versioned<? extends DiaryRecord>> records)
			{
				synchronized (data)
				{
					Date prevDate = getLastDate();

					if (!records.isEmpty())
					{
						for (int i = 0; i < records.size(); i++)
						{
							Versioned<? extends DiaryRecord> record = records.get(i);
							if (!sameDate(prevDate, record.getData().getTime()))
							{
								// TODO: it's time, not the date
								data.add(new ItemHeader(record.getData().getTime()));
							}
							data.add(new ItemData(record));
							prevDate = record.getData().getTime();
						}
					}
					else
					{
						data.add(new ItemHeader(Utils.getNextDay(getLastDate())));
					}

					loadingAfter = false;
					((BaseAdapter) ((HeaderViewListAdapter) listRecs.getAdapter()).getWrappedAdapter())
							.notifyDataSetChanged();
				}
			}
		}.execute(date);
	}

	List<Versioned<? extends DiaryRecord>> request(Date startTime, Date endTime)
	{
		List<Versioned<DiaryRecord>> temp = diaryService.findPeriod(startTime, endTime, false);

		List<Versioned<? extends DiaryRecord>> result = new ArrayList<Versioned<? extends DiaryRecord>>();

		for (Versioned<DiaryRecord> item : temp)
		{
			if (item.getData() instanceof BloodRecord || item.getData() instanceof InsRecord)
			{
				result.add(item);
			}
		}

		return result;
	}
}