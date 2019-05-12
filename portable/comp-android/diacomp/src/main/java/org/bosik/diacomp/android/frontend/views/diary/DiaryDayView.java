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

import android.content.Context;
import android.os.AsyncTask;
import android.util.AttributeSet;
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
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.PostprandUtils;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;

public class DiaryDayView extends LinearLayout
{
	private static final long SEPARATOR_TIMEOUT = PostprandUtils.DEFAULT_AFFECT_TIME_MAX * Utils.MsecPerMin;

	private final PreferencesTypedService preferences;

	private static abstract class Item
	{
		public abstract Date extractDate();
	}

	private static class ItemHeader extends Item
	{
		public Date date;

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
		public Versioned<DiaryRecord> record;

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

	private static class ItemSeparator extends Item
	{
		private final Date date;

		public ItemSeparator(Date date)
		{
			this.date = date;
		}

		@Override
		public Date extractDate()
		{
			return date;
		}
	}

	public interface OnRecordClickListener
	{
		void onRecordClick(Versioned<DiaryRecord> record);
	}

	public interface OnHeaderClickListener
	{
		void onHeaderClick(Date date);
	}

	// Data
	private Date firstDate;
	private int  countOfDays;
	private final List<Item> data          = new ArrayList<>();
	private       boolean    loading       = false;
	private       boolean    loadingBefore = false;
	private       boolean    loadingAfter  = false;
	private int offset;

	// Components
	private BaseAdapter adapter;
	private ListView    listRecs;

	// Listeners
	private OnRecordClickListener onRecordClickListener;
	private OnHeaderClickListener onHeaderClickListener;

	public DiaryDayView(final Context context)
	{
		this(context, null);
	}

	private int positionToIndex(int position)
	{
		return position - offset;
	}

	public DiaryDayView(final Context context, AttributeSet attributes)
	{
		super(context, attributes);
		final LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_day, this);

		preferences = new PreferencesTypedService(new PreferencesLocalService(context));

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
				List<Versioned<DiaryRecord>> removedRecords = new ArrayList<>();

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
								itemData.record.modified();
								removedRecords.add(itemData.record);
							}
						}
					}
				}

				LocalDiary.getInstance(getContext()).save(removedRecords);

				String text = String.format(Locale.US, getContext().getString(R.string.common_tip_deleted), removedRecords.size());
				UIUtils.showTipLong(listRecs.getContext(), text);

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
			static final int TYPE_HEADER = 1;
			static final int TYPE_DATA = 2;
			static final int TYPE_PROGRESS = 3;
			static final int UNKNOWN = -17;

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
					textTitle.setText(UIUtils.formatDateLocalDevice(DiaryDayView.this.getContext(), ((ItemHeader) item).date));
				}
				else if (item instanceof ItemData)
				{
					Versioned<? extends DiaryRecord> record = ((ItemData) item).record;
					DiaryRecord data = record.getData();
					if (data instanceof BloodRecord)
					{
						convertView = new DiaryRecBloodView(context, (Versioned<BloodRecord>) record);
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
				else if (item instanceof ItemSeparator)
				{
					if (convertView == null)
					{
						convertView = inflater.inflate(R.layout.view_diary_rec_separator, null);
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

	private void setDate(Date date)
	{
		synchronized (data)
		{
			firstDate = date;
			countOfDays = 1;
			refresh();
		}
	}

	private List<Item> groupItems(List<Versioned<DiaryRecord>> records, Date firstDate, int countOfDays)
	{
		List<Item> result = new ArrayList<>();

		final boolean useSeparator = preferences.getBooleanValue(PreferenceID.ANDROID_DIARY_USE_SEPARATOR);
		Date curDate = firstDate;
		int index = 0;

		for (int i = 1; i <= countOfDays; i++, curDate = Utils.getNextDay(curDate))
		{
			// TODO: it's time, not the date
			result.add(new ItemHeader(curDate));
			boolean firstRecordOfDay = true;
			while (index < records.size() && Utils.sameDay(curDate, records.get(index).getData().getTime()))
			{
				if (useSeparator && index > 0 && !firstRecordOfDay)
				{
					final long timeA = records.get(index - 1).getData().getTime().getTime();
					final long timeB = records.get(index).getData().getTime().getTime();
					if (timeB - timeA >= SEPARATOR_TIMEOUT)
					{
						final Date date = new Date((timeA + timeB) / 2);
						result.add(new ItemSeparator(date));
					}
				}
				result.add(new ItemData(records.get(index)));
				index++;
				firstRecordOfDay = false;
			}
		}

		return result;
	}

	public void refresh()
	{
		if (loading || loadingBefore || loadingAfter)
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
				final List<Versioned<DiaryRecord>> records = loadData(timeFrom, timeTo);
				return groupItems(records, timeFrom, days);
			}

			@Override
			protected void onPostExecute(List<Item> items)
			{
				synchronized (data)
				{
					data.clear();
					data.addAll(items);
					updatePostprand();
				}
				adapter.notifyDataSetChanged();
				loading = false;
			}
		}.execute(timeFrom, timeTo);
	}

	private void loadBefore(final int days)
	{
		if (loading || loadingBefore || loadingAfter)
		{
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
				final List<Versioned<DiaryRecord>> records = loadData(timeFrom, timeTo);
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

				firstDate = timeFrom;
				countOfDays += days;

				loadingBefore = false;

				offset -= delta;
				adapter.notifyDataSetChanged();
			}
		}.execute(timeFrom, timeTo);
	}

	private void loadAfter(final int days)
	{
		if (loading || loadingBefore || loadingAfter)
		{
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
				final List<Versioned<DiaryRecord>> records = loadData(timeFrom, timeTo);
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

	private List<Versioned<DiaryRecord>> loadData(Date timeFrom, Date timeTo)
	{
		final DiaryService diaryService = LocalDiary.getInstance(getContext());
		return diaryService.findPeriod(timeFrom, timeTo, false);
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
					MealRecord meal = (MealRecord) record;
					if (meal.getCarbs() > 1.0)
					{
						long affectTime = meal.getShortMeal() ? DEFAULT_AFFECT_TIME_MEAL_SHORT : DEFAULT_AFFECT_TIME_MEAL_STD;
						long curFreeTime = record.getTime().getTime() + affectTime * Utils.MsecPerMin;
						if (curFreeTime > minFreeTime)
						{
							minFreeTime = curFreeTime;
						}
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