/*  
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *  
 */
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
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import android.content.Context;
import android.os.AsyncTask;
import android.util.AttributeSet;
import android.util.Log;
import android.util.SparseBooleanArray;
import android.view.ActionMode;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AbsListView.MultiChoiceModeListener;
import android.widget.AbsListView.OnScrollListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.BaseAdapter;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

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
		public Versioned<DiaryRecord>	record;

		public ItemData(Versioned<DiaryRecord> record)
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
		void onRecordClick(Versioned<DiaryRecord> record);
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
		listRecs.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE_MODAL);
		listRecs.setMultiChoiceModeListener(new MultiChoiceModeListener()
		{
			@Override
			public void onItemCheckedStateChanged(ActionMode actionMode, int i, long l, boolean b)
			{
				int selectedCount = listRecs.getCheckedItemCount();
				setSubtitle(actionMode, selectedCount);
			}

			@Override
			public boolean onCreateActionMode(ActionMode actionMode, Menu menu)
			{
				MenuInflater inflater = actionMode.getMenuInflater();
				inflater.inflate(R.menu.actions_diary_context, menu);
				return true;
			}

			@Override
			public boolean onPrepareActionMode(ActionMode actionMode, Menu menu)
			{
				return false;
			}

			@Override
			public boolean onActionItemClicked(ActionMode actionMode, MenuItem menuItem)
			{
				List<Versioned<DiaryRecord>> removedRecords = new ArrayList<Versioned<DiaryRecord>>();

				SparseBooleanArray sparseBooleanArray = listRecs.getCheckedItemPositions();
				for (int i = 0; i < sparseBooleanArray.size(); i++)
				{
					if (sparseBooleanArray.valueAt(i))
					{
						int position = sparseBooleanArray.keyAt(i);
						int index = positionToIndex(position);
						if (index >= 0 && index < data.size())
						{
							Item item = data.get(index);
							if (item instanceof ItemData)
							{
								ItemData itemData = (ItemData) item;
								itemData.record.setDeleted(true);
								itemData.record.updateTimeStamp();
								removedRecords.add(itemData.record);
							}
						}
					}
				}

				diaryService.save(removedRecords);

				// TODO: i18n
				String text = removedRecords.size() + " items removed";
				Toast.makeText(listRecs.getContext(), text, Toast.LENGTH_LONG).show();

				return true;
			}

			@Override
			public void onDestroyActionMode(ActionMode actionMode)
			{
			}

			private void setSubtitle(ActionMode mode, int selectedCount)
			{
				switch (selectedCount)
				{
					case 0:
						mode.setSubtitle(null);
						break;
					default:
						mode.setTitle(String.valueOf(selectedCount));
						break;
				}
			}
		});

		Calendar c = Calendar.getInstance();
		c.set(Calendar.HOUR_OF_DAY, 0);
		c.set(Calendar.MINUTE, 0);
		c.set(Calendar.SECOND, 0);
		c.set(Calendar.MILLISECOND, 0);

		setDate(c.getTime());

		// ================================================================

		adapter = new BaseAdapter()
		{
			static final int	TYPE_HEADER		= 1;
			static final int	TYPE_DATA		= 2;
			static final int	TYPE_PROGRESS	= 3;
			static final int	UNKNOWN			= -17;

			@Override
			public int getViewTypeCount()
			{
				return 4;
			}

			@Override
			public int getItemViewType(int position)
			{
				final Object item = getItem(position);

				if (item == null)
				{
					return TYPE_PROGRESS;
				}
				else if (item instanceof ItemHeader)
				{
					return TYPE_HEADER;
				}
				else if (item instanceof ItemData)
				{
					return TYPE_DATA;
				}
				else
				{
					return UNKNOWN;
				}
			}

			@Override
			public boolean isEnabled(int position)
			{
				return (getItemViewType(position) == TYPE_DATA);
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
						DiaryRecBloodView rec = new DiaryRecBloodView(context, (Versioned<BloodRecord>) record);
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
					Versioned<DiaryRecord> record = ((ItemData) item).record;
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

	List<Item> groupItems(List<Versioned<DiaryRecord>> records, Date firstDate, int countOfDays)
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
				List<Versioned<DiaryRecord>> records = request(timeFrom, timeTo);
				return groupItems(records, timeFrom, days);
			}

			@Override
			protected void onPostExecute(List<Item> items)
			{
				synchronized (data)
				{
					data = items;
					updatePostprand();
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
				final List<Versioned<DiaryRecord>> records = request(timeFrom, timeTo);
				return groupItems(records, timeFrom, days);
			}

			@Override
			protected void onPostExecute(List<Item> items)
			{
				synchronized (data)
				{
					data.addAll(0, items);
					updatePostprand();
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
				final List<Versioned<DiaryRecord>> records = request(timeFrom, timeTo);
				return groupItems(records, timeFrom, days);
			}

			@Override
			protected void onPostExecute(List<Item> items)
			{
				synchronized (data)
				{
					data.addAll(items);
					updatePostprand();
				}
				countOfDays += days;

				loadingAfter = false;
				adapter.notifyDataSetChanged();
			}
		}.execute(timeFrom, timeTo);
	}

	static List<Versioned<DiaryRecord>> request(Date startTime, Date endTime)
	{
		List<Versioned<DiaryRecord>> result = new ArrayList<Versioned<DiaryRecord>>();
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

	private void updatePostprand()
	{
		// TODO: move hardcode
		final int DEFAULT_AFFECT_TIME_INSULIN = 210;
		final int DEFAULT_AFFECT_TIME_MEAL_STD = 210;
		final int DEFAULT_AFFECT_TIME_MEAL_SHORT = 20;
		long minFreeTime = 0;

		for (Item item : data)
		{
			if (item instanceof ItemData)
			{
				ItemData itemData = (ItemData) item;
				DiaryRecord record = itemData.record.getData();
				if (record instanceof InsRecord)
				{
					long curFreeTime = record.getTime().getTime() + DEFAULT_AFFECT_TIME_INSULIN * Utils.MsecPerMin;
					if (curFreeTime > minFreeTime)
					{
						minFreeTime = curFreeTime;
					}
				}
				else if (record instanceof MealRecord)
				{
					long affectTime = ((MealRecord) record).getShortMeal() ? DEFAULT_AFFECT_TIME_MEAL_SHORT
							: DEFAULT_AFFECT_TIME_MEAL_STD;
					long curFreeTime = record.getTime().getTime() + affectTime * Utils.MsecPerMin;
					if (curFreeTime > minFreeTime)
					{
						minFreeTime = curFreeTime;
					}
				}
				else if (record instanceof BloodRecord)
				{
					((BloodRecord) record).setPostPrand(record.getTime().getTime() < minFreeTime);
				}
			}
		}
	}
}