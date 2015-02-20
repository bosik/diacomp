package org.bosik.diacomp.android.frontend.fragments;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService.Verifier;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.views.diary.DiaryView;
import org.bosik.diacomp.android.frontend.views.diary.RecordClickListener;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ScrollView;

public class FragmentDiary extends Fragment
{
	private static final String			TAG							= FragmentDiary.class.getSimpleName();

	public static final String			KEY_DATE					= "diacomp.diary.date";

	private static final int			CONTEXT_ITEM_EDIT			= 0;
	private static final int			CONTEXT_ITEM_REMOVE			= 1;

	// FIXME: hardcoded patient params
	private static final long			SCAN_FOR_BLOOD_FINGER		= 5 * Utils.SecPerDay;
	private static final int			SCAN_FOR_BLOOD_BEFORE_MEAL	= 4 * Utils.SecPerHour;
	private static final long			SCAN_FOR_INS_AROUND_MEAL	= 3 * Utils.SecPerHour;
	private final Double				bloodTarget					= 5.0;

	// THINK: что произойдёт на смене дат?
	static Date							curDate						= Calendar.getInstance().getTime();
	static List<Versioned<DiaryRecord>>	curRecords					= null;

	// --- форматы ---
	// private final SimpleDateFormat CaptionFmt = new SimpleDateFormat("d MMMM");
	private final SimpleDateFormat		FORMAT_DATE					= new SimpleDateFormat("dd.MM.yyyy", Locale.US);

	// Services
	private DiaryService				diary;

	// Components
	private DiaryView					diaryViewLayout;
	private Button						buttonPrevDay;
	private Button						buttonNextDay;
	private Button						buttonSelectDay;

	private ScrollView					scrollView;

	// handled
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		View rootView = inflater.inflate(R.layout.fragment_diary, container, false);

		try
		{
			setHasOptionsMenu(true);

			// services
			diary = new DiaryLocalService(getActivity().getContentResolver());

			// UI: components

			diaryViewLayout = (DiaryView) rootView.findViewById(R.id.diaryViewLayout);
			registerForContextMenu(diaryViewLayout);
			buttonPrevDay = (Button) rootView.findViewById(R.id.buttonPrevDay);
			buttonNextDay = (Button) rootView.findViewById(R.id.buttonNextDay);
			buttonSelectDay = (Button) rootView.findViewById(R.id.buttonSelectDay);

			scrollView = (ScrollView) rootView.findViewById(R.id.scrollView);

			// UI: events handling

			buttonPrevDay.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					openPage(Utils.getPrevDay(curDate));
				}
			});
			buttonNextDay.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					openPage(Utils.getNextDay(curDate));
				}
			});
			buttonSelectDay.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					// ((Button)v).setText("it's ok");
				}
			});

			scrollView.setBackgroundResource(R.color.app_background);
			diaryViewLayout.setOnRecordClickListener(new RecordClickListener()
			{
				// handled
				@SuppressWarnings("unchecked")
				@Override
				public void onRecordClick(int index)
				{
					// try
					// {
					// Versioned<? extends DiaryRecord> rec = curRecords.get(index);
					//
					// if (rec.getData() instanceof BloodRecord)
					// {
					// showBloodEditor((Versioned<BloodRecord>) rec, false);
					// }
					// else if (rec.getData() instanceof InsRecord)
					// {
					// showInsEditor((Versioned<InsRecord>) rec, false);
					// }
					// else if (rec.getData() instanceof MealRecord)
					// {
					// showMealEditor((Versioned<MealRecord>) rec, false);
					// }
					// else if (rec.getData() instanceof NoteRecord)
					// {
					// showNoteEditor((Versioned<NoteRecord>) rec, false);
					// }
					// }
					// catch (Exception e)
					// {
					// ErrorHandler.handle(e, getActivity());
					// }
				}
			});

			// определение параметров запроса
			// Intent intent = getIntent();

			/*
			 * if (intent.hasExtra(KEY_DATE)) { Date date = (Date)
			 * intent.getSerializableExtra(KEY_DATE); openPage(date); Log.d(TAG,
			 * "DiaryView(): onCreate(), date is specified: " + date); } else
			 */
			{
				openPage(curDate);
				Log.d(TAG, "DiaryView(): onCreate(), date default: " + curDate);
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, getActivity());
		}

		return rootView;
	}

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
	{
		inflater.inflate(R.menu.actions_diary, menu);
		super.onCreateOptionsMenu(menu, inflater);
	}

	// handled
	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo)
	{
		try
		{
			if (v.getId() == diaryViewLayout.getId())
			{
				if ((DiaryView.getDownedIndex() < 0) || (DiaryView.getDownedIndex() >= curRecords.size()))
				{
					return;
				}

				DiaryRecord rec = curRecords.get(DiaryView.getDownedIndex()).getData();
				Class<?> c = rec.getClass();

				if (c == BloodRecord.class)
				{
					menu.setHeaderTitle(getString(R.string.common_rectype_blood));
					menu.add(Menu.NONE, CONTEXT_ITEM_EDIT, 0, getString(R.string.diary_context_common_edit));
					menu.add(Menu.NONE, CONTEXT_ITEM_REMOVE, 1, getString(R.string.diary_context_common_remove));
				}
				else if (c == InsRecord.class)
				{
					menu.setHeaderTitle(getString(R.string.common_rectype_ins));
					menu.add(Menu.NONE, CONTEXT_ITEM_EDIT, 0, getString(R.string.diary_context_common_edit));
					menu.add(Menu.NONE, CONTEXT_ITEM_REMOVE, 1, getString(R.string.diary_context_common_remove));
				}
				else if (c == MealRecord.class)
				{
					menu.setHeaderTitle(getString(R.string.common_rectype_meal));
					menu.add(Menu.NONE, CONTEXT_ITEM_EDIT, 0, getString(R.string.diary_context_common_edit));
					menu.add(Menu.NONE, CONTEXT_ITEM_REMOVE, 1, getString(R.string.diary_context_common_remove));
				}
				else if (c == NoteRecord.class)
				{
					menu.setHeaderTitle(getString(R.string.common_rectype_note));
					menu.add(Menu.NONE, CONTEXT_ITEM_EDIT, 0, getString(R.string.diary_context_common_edit));
					menu.add(Menu.NONE, CONTEXT_ITEM_REMOVE, 1, getString(R.string.diary_context_common_remove));
				}
				else
				{
					// TODO: localize
					UIUtils.showTip(getActivity(), "Unsupported record type");
				}
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, getActivity());
		}
	}

	// handled
	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		try
		{
			switch (item.getItemId())
			{
				default:
					return super.onOptionsItemSelected(item);
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, getActivity());
		}

		return true;
	}

	// handled
	@SuppressWarnings("unchecked")
	@Override
	public boolean onContextItemSelected(MenuItem item)
	{
		// try
		// {
		// final int ind = DiaryView.getDownedIndex();
		//
		// if ((ind < 0) || (ind >= curRecords.size()))
		// {
		// return false;
		// }
		//
		// // Common options
		//
		// switch (item.getItemId())
		// {
		// case CONTEXT_ITEM_REMOVE:
		// {
		// String id = curRecords.get(ind).getId();
		// diary.delete(id);
		// openPage(curDate);
		//
		// return true;
		// }
		// }
		//
		// // Type-specific options
		//
		// Versioned<? extends DiaryRecord> rec = curRecords.get(ind);
		// Class<?> c = rec.getClass();
		//
		// if (c == BloodRecord.class)
		// {
		// switch (item.getItemId())
		// {
		// case CONTEXT_ITEM_EDIT:
		// {
		// Versioned<BloodRecord> temp = (Versioned<BloodRecord>) rec;
		// showBloodEditor(temp, false);
		// return true;
		// }
		// default:
		// {
		// UIUtils.showTip(getActivity(), "Unsupported option");
		// return true;
		// }
		// }
		// }
		//
		// else
		//
		// if (c == InsRecord.class)
		// {
		// switch (item.getItemId())
		// {
		// case CONTEXT_ITEM_EDIT:
		// {
		// Versioned<InsRecord> temp = (Versioned<InsRecord>) rec;
		// showInsEditor(temp, false);
		// return true;
		// }
		// default:
		// {
		// UIUtils.showTip(getActivity(), "Unsupported option");
		// return true;
		// }
		// }
		// }
		//
		// else
		//
		// if (c == MealRecord.class)
		// {
		// switch (item.getItemId())
		// {
		// case CONTEXT_ITEM_EDIT:
		// {
		// Versioned<MealRecord> temp = (Versioned<MealRecord>) rec;
		// showMealEditor(temp, false);
		// return true;
		// }
		// default:
		// {
		// UIUtils.showTip(getActivity(), "Unsupported option");
		// return true;
		// }
		// }
		// }
		//
		// else
		//
		// if (c == NoteRecord.class)
		// {
		// switch (item.getItemId())
		// {
		// case CONTEXT_ITEM_EDIT:
		// {
		// Versioned<NoteRecord> temp = (Versioned<NoteRecord>) rec;
		// showNoteEditor(temp, false);
		// return true;
		// }
		// default:
		// {
		// UIUtils.showTip(getActivity(), "Unsupported option");
		// return true;
		// }
		// }
		// }
		//
		// else
		//
		// {
		// UIUtils.showTip(getActivity(), "Unsupported record type");
		// return true;
		// }
		// }
		// catch (Exception e)
		// {
		// ErrorHandler.handle(e, getActivity());
		// }

		return true;
	}

	// ACTIONS

	void openPage(Date date)
	{
		curDate = date;

		Calendar c = Calendar.getInstance();
		c.setTime(curDate);
		c.set(Calendar.HOUR_OF_DAY, 0);
		c.set(Calendar.MINUTE, 0);
		c.set(Calendar.SECOND, 0);
		c.set(Calendar.MILLISECOND, 0);

		Date start = c.getTime();
		Date end = Utils.getNextDay(start);

		curRecords = diary.findPeriod(start, end, false);

		if (!Verifier.verifyRecords(curRecords, start, end))
		{
			UIUtils.showTip(getActivity(), "Diary verification failed");
		}

		diaryViewLayout.setRecords(curRecords);
		buttonSelectDay.setText(FORMAT_DATE.format(curDate));
	}
}
