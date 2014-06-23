package org.bosik.diacomp.android.frontend.activities;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
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
import org.bosik.diacomp.core.utils.Utils;
import android.app.Activity;
import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

public class ActivityDiary extends Activity implements RecordClickListener, OnClickListener
{
	private static final String					TAG							= ActivityDiary.class.getSimpleName();

	private static final int					DIALOG_BLOOD_CREATE			= 11;
	private static final int					DIALOG_BLOOD_MODIFY			= 12;
	private static final int					DIALOG_INS_CREATE			= 21;
	private static final int					DIALOG_INS_MODIFY			= 22;
	private static final int					DIALOG_MEAL_CREATE			= 31;
	private static final int					DIALOG_MEAL_MODIFY			= 32;
	private static final int					DIALOG_NOTE_CREATE			= 41;
	private static final int					DIALOG_NOTE_MODIFY			= 42;

	private static final int					CONTEXT_ITEM_EDIT			= 0;
	private static final int					CONTEXT_ITEM_REMOVE			= 1;

	private static final long					SCAN_FOR_BLOOD_FINGER		= 5 * Utils.SecPerDay;
	private static final int					SCAN_FOR_BLOOD_BEFORE_MEAL	= 4 * Utils.SecPerHour;
	private static final long					SCAN_FOR_INS_AROUND_MEAL	= 3 * Utils.SecPerHour;

	// FIXME: hardcoded target BS
	private final Double						bloodTarget					= 5.0;

	// THINK: что произойдёт на смене дат?
	private static Date							curDate						= Calendar.getInstance().getTime();
	private static List<Versioned<DiaryRecord>>	curRecords					= null;

	// --- форматы ---
	// private static final SimpleDateFormat CaptionFmt = new SimpleDateFormat("d MMMM");
	private final SimpleDateFormat				CaptionFmt					= new SimpleDateFormat("dd.MM.yyyy",
																					Locale.US);

	// КОМПОНЕНТЫ
	private DiaryView							diaryViewLayout;
	private Button								buttonSelectDay;

	// СОБЫТИЯ

	// handled
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		try
		{
			// Log.i(TAG, "DiaryView(): onCreate()");
			setContentView(R.layout.activity_diary);

			// определяем компоненты
			diaryViewLayout = (DiaryView) findViewById(R.id.diaryViewLayout);
			buttonSelectDay = (Button) findViewById(R.id.buttonSelectDay);

			// НАСТРОЙКА СИСТЕМЫ
			registerForContextMenu(diaryViewLayout);

			// назначаем обработчики
			((Button) findViewById(R.id.buttonPrevDay)).setOnClickListener(this);
			buttonSelectDay.setOnClickListener(this);
			((Button) findViewById(R.id.buttonNextDay)).setOnClickListener(this);
			((Button) findViewById(R.id.buttonAddBlood)).setOnClickListener(this);
			((Button) findViewById(R.id.buttonAddIns)).setOnClickListener(this);
			((Button) findViewById(R.id.buttonAddMeal)).setOnClickListener(this);
			((Button) findViewById(R.id.buttonAddNote)).setOnClickListener(this);
			findViewById(R.id.scrollView).setBackgroundColor(Color.WHITE);
			diaryViewLayout.setOnRecordClickListener(this);

			// определение параметров запроса
			// Intent intent = getIntent();

			/*
			 * if (intent.hasExtra("date")) { Date date = (Date)
			 * intent.getSerializableExtra("date"); openPage(date); Log.d(TAG,
			 * "DiaryView(): onCreate(), date is specified: " + date); } else
			 */
			{
				openPage(curDate);
				Log.d(TAG, "DiaryView(): onCreate(), date default: " + curDate);
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// handled
	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		try
		{
			MenuInflater inflater = getMenuInflater();
			inflater.inflate(R.menu.diary_menu, menu);
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
		return true;
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
					UIUtils.showTip(this, "Unsupported record type");
				}
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// handled
	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		// FIXME: ########################### UNUSED ##############################

		try
		{
			switch (item.getItemId())
			{
				case R.id.item_diary_addblood:
				{
					showBloodEditor(null, true);
					return true;
				}
				case R.id.item_diary_addins:
				{
					showInsEditor(null, true);
					return true;
				}
				case R.id.item_diary_addmeal:
				{
					// TODO: handler
					UIUtils.showTip(this, "Creating meal is not implemented yet");

					/*
					 * Date date = Calendar.getInstance().getTime(); Log.i(TAG,
					 * "menuClick: downloading page..."); DiaryPage page =
					 * Storage.webDiary.getPage(date); Date timestamp =
					 * Storage.webDiary.diarySource.getTimeStamp(date);
					 * 
					 * if (null != page) { Log.d(TAG, "menuClick: page is ok");
					 * Storage.localDiary.postPage(page, timestamp); } else Log.d(TAG,
					 * "menuClick: page is null");
					 * 
					 * Log.d(TAG, "menuClick: invalidating...");
					 * 
					 * //DiaryView.curPage = page; //mDiaryView.invalidate();
					 * mDiaryView.setPage(page);
					 */

					return true;
				}
				case R.id.item_diary_addnote:
				{
					UIUtils.showTip(this, "Creating note is not implemented yet");
					/*
					 * Log.i(TAG, "menuClick: opening page..."); DiaryPage page =
					 * Storage.localDiary.getPage(curDate);
					 * 
					 * if (null != page) Log.d(TAG, "menuClick: page is ok"); else Log.d(TAG,
					 * "menuClick: page is null");
					 * 
					 * Log.d(TAG, "menuClick: invalidating...");
					 * 
					 * //DiaryView.curPage = page; //mDiaryView.invalidate();
					 * mDiaryView.setPage(page);
					 * 
					 * 
					 * DiaryPage page = getCurPage();
					 * 
					 * // добавляем int minTime = Utils.curMinutes(); NoteRecord note = new
					 * NoteRecord(minTime, "текст"); page.add(note); // сохраняем postPage(page);
					 */

					return true;
				}
				// TODO: other handlers

				default:
					return super.onOptionsItemSelected(item);
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}

		return true;
	}

	// handled
	@SuppressWarnings("unchecked")
	@Override
	public boolean onContextItemSelected(MenuItem item)
	{
		try
		{
			final int ind = DiaryView.getDownedIndex();

			if ((ind < 0) || (ind >= curRecords.size()))
			{
				return false;
			}

			// Common options

			switch (item.getItemId())
			{
				case CONTEXT_ITEM_REMOVE:
				{
					curRecords.get(ind).setDeleted(true);
					curRecords.get(ind).updateTimeStamp();
					postRecord(curRecords.get(ind));
					return true;
				}
			}

			// Type-specific options

			Versioned<? extends DiaryRecord> rec = curRecords.get(ind);
			Class<?> c = rec.getClass();

			if (c == BloodRecord.class)
			{
				switch (item.getItemId())
				{
					case CONTEXT_ITEM_EDIT:
					{
						Versioned<BloodRecord> temp = (Versioned<BloodRecord>) rec;
						showBloodEditor(temp, false);
						return true;
					}
					default:
					{
						UIUtils.showTip(this, "Unsupported option");
						return true;
					}
				}
			}

			else

			if (c == InsRecord.class)
			{
				switch (item.getItemId())
				{
					case CONTEXT_ITEM_EDIT:
					{
						Versioned<InsRecord> temp = (Versioned<InsRecord>) rec;
						showInsEditor(temp, false);
						return true;
					}
					default:
					{
						UIUtils.showTip(this, "Unsupported option");
						return true;
					}
				}
			}

			else

			if (c == MealRecord.class)
			{
				switch (item.getItemId())
				{
					case CONTEXT_ITEM_EDIT:
					{
						Versioned<MealRecord> temp = (Versioned<MealRecord>) rec;
						showMealEditor(temp, false);
						return true;
					}
					default:
					{
						UIUtils.showTip(this, "Unsupported option");
						return true;
					}
				}
			}

			else

			if (c == NoteRecord.class)
			{
				switch (item.getItemId())
				{
					case CONTEXT_ITEM_EDIT:
					{
						Versioned<NoteRecord> temp = (Versioned<NoteRecord>) rec;
						showNoteEditor(temp, false);
						return true;
					}
					default:
					{
						UIUtils.showTip(this, "Unsupported option");
						return true;
					}
				}
			}

			// TODO: implement other record types

			else

			{
				UIUtils.showTip(this, "Unsupported record type");
				return true;
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}

		return true;
	}

	// handled
	@Override
	public void onClick(View v)
	{
		try
		{
			switch (v.getId())
			{
				case R.id.buttonPrevDay:
				{
					openPage(Utils.getPrevDay(curDate));
					break;
				}
				case R.id.buttonNextDay:
				{
					openPage(Utils.getNextDay(curDate));
					break;
				}
				case R.id.buttonSelectDay:
				{
					// ...
					// ((Button)v).setText("it's ok");
					break;
				}
				case R.id.buttonAddBlood:
				{
					showBloodEditor(null, true);
					break;
				}

				case R.id.buttonAddIns:
				{
					showInsEditor(null, true);
					break;
				}

				case R.id.buttonAddMeal:
				{
					showMealEditor(null, true);
					break;
				}

				case R.id.buttonAddNote:
				{
					showNoteEditor(null, true);
					break;
				}
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// handled
	@SuppressWarnings("unchecked")
	@Override
	public void onRecordClick(int index)
	{
		try
		{
			Versioned<? extends DiaryRecord> rec = curRecords.get(index);

			if (rec.getData().getClass() == BloodRecord.class)
			{
				showBloodEditor((Versioned<BloodRecord>) rec, false);
			}
			else if (rec.getData().getClass() == InsRecord.class)
			{
				showInsEditor((Versioned<InsRecord>) rec, false);
			}
			else if (rec.getData().getClass() == MealRecord.class)
			{
				showMealEditor((Versioned<MealRecord>) rec, false);
			}
			else if (rec.getData().getClass() == NoteRecord.class)
			{
				showNoteEditor((Versioned<NoteRecord>) rec, false);
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	private void setCaptionDate(Date date)
	{
		String s = CaptionFmt.format(date);
		setTitle(String.format("%s (%s)", getString(R.string.diary_title), s));
		buttonSelectDay.setText(s);
	}

	// handled
	@SuppressWarnings("unchecked")
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent intent)
	{
		super.onActivityResult(requestCode, resultCode, intent);

		try
		{
			switch (requestCode)
			{
				case DIALOG_BLOOD_CREATE:
				case DIALOG_INS_CREATE:
				case DIALOG_MEAL_CREATE:
				case DIALOG_NOTE_CREATE:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<DiaryRecord> rec = (Versioned<DiaryRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						// TODO: inserting should be implemented here (not updating)
						postRecord(rec);
					}
					break;
				}

				case DIALOG_BLOOD_MODIFY:
				case DIALOG_INS_MODIFY:
				case DIALOG_MEAL_MODIFY:
				case DIALOG_NOTE_MODIFY:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<DiaryRecord> rec = (Versioned<DiaryRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						postRecord(rec);
					}
					break;
				}
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// ДЕЙСТВИЯ

	private void openPage(Date date)
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

		curRecords = Storage.localDiary.findBetween(start, end, false);
		Log.d(TAG,
				String.format("Found %d items between %s and %s", curRecords.size(), Utils.formatTimeUTC(start),
						Utils.formatTimeUTC(end)));
		diaryViewLayout.setRecords(curRecords);
		setCaptionDate(curDate);
	}

	public void postRecord(Versioned<DiaryRecord> rec)
	{
		List<Versioned<DiaryRecord>> list = new LinkedList<Versioned<DiaryRecord>>();
		list.add(rec);
		Storage.localDiary.save(list);
		openPage(curDate);
	}

	/**
	 * Searches for the last BS record in period [since - scanPeriod, since]
	 * 
	 * @param scanPeriod
	 *            , in seconds
	 * @param since
	 * @return
	 */
	private static BloodRecord lastBlood(long scanPeriod, Date since, boolean skipPostprandials)
	{
		// TODO: move this away from UI
		Date toDate = since;
		Date fromDate = new Date(toDate.getTime() - (scanPeriod * Utils.MsecPerSec));

		List<Versioned<DiaryRecord>> records = Storage.localDiary.findBetween(fromDate, toDate, false);
		Collections.reverse(records);

		for (Versioned<DiaryRecord> record : records)
		{
			if ((record.getData().getClass() == BloodRecord.class)
					&& (!skipPostprandials || !((BloodRecord) record.getData()).isPostPrand()))
			{
				return (BloodRecord) record.getData();
			}
		}

		return null;
	}

	/**
	 * Searches for the insulin injection nearest to the specified time
	 * 
	 * @param near
	 *            Time to search around
	 * @param scanPeriod
	 *            , in seconds
	 * @return
	 */
	private static InsRecord findInsulin(Date near, long scanPeriod)
	{
		Log.d(TAG, "findInsulin(): near = " + near + ", scanPeriod = " + scanPeriod);

		// TODO: move this away from UI

		Date fromDate = new Date(near.getTime() - (scanPeriod * Utils.MsecPerSec));
		Date toDate = new Date(near.getTime() + (scanPeriod * Utils.MsecPerSec));
		List<Versioned<DiaryRecord>> records = Storage.localDiary.findBetween(fromDate, toDate, false);

		Log.d(TAG, "findInsulin(): fromDate = " + fromDate);
		Log.d(TAG, "findInsulin(): toDate = " + toDate);
		Log.d(TAG, "findInsulin(): items found: " + records.size());

		long min = scanPeriod * Utils.MsecPerSec * 2;
		InsRecord ins = null;

		for (Versioned<DiaryRecord> record : records)
		{
			if (record.getData().getClass() == InsRecord.class)
			{
				long currentDist = Math.abs(record.getData().getTime().getTime() - near.getTime());
				if (currentDist < min)
				{
					ins = (InsRecord) record.getData();
				}
			}
		}

		return ins;
	}

	private void showBloodEditor(Versioned<BloodRecord> entity, boolean createMode)
	{
		if (createMode)
		{
			BloodRecord prev = lastBlood(SCAN_FOR_BLOOD_FINGER, new Date(), false);
			BloodRecord rec = new BloodRecord();
			rec.setTime(new Date());
			rec.setFinger(((prev == null) || (prev.getFinger() == -1)) ? -1 : ((prev.getFinger() + 1) % 10));
			entity = new Versioned<BloodRecord>(rec);
		}

		Intent intent = new Intent(this, ActivityEditorBlood.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);

		startActivityForResult(intent, createMode ? DIALOG_BLOOD_CREATE : DIALOG_BLOOD_MODIFY);
	}

	private void showInsEditor(Versioned<InsRecord> entity, boolean createMode)
	{
		if (createMode)
		{
			InsRecord rec = new InsRecord();
			rec.setTime(new Date());
			entity = new Versioned<InsRecord>(rec);
		}

		Intent intent = new Intent(this, ActivityEditorIns.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);

		startActivityForResult(intent, createMode ? DIALOG_INS_CREATE : DIALOG_INS_MODIFY);
	}

	private void showMealEditor(Versioned<MealRecord> entity, boolean createMode)
	{
		if (createMode)
		{
			MealRecord rec = new MealRecord();
			rec.setTime(new Date());
			entity = new Versioned<MealRecord>(rec);
		}

		BloodRecord prevBlood = lastBlood(SCAN_FOR_BLOOD_BEFORE_MEAL, entity.getData().getTime(), true);
		Double bloodBeforeMeal = prevBlood == null ? null : prevBlood.getValue();
		InsRecord insRecord = findInsulin(entity.getData().getTime(), SCAN_FOR_INS_AROUND_MEAL);
		Double insInjected = insRecord == null ? null : insRecord.getValue();

		Log.d(TAG, insRecord == null ? "insRecord == null" : "insRecord != null");
		Log.d(TAG, "insInjected: " + insInjected);

		Intent intent = new Intent(this, ActivityEditorMeal.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);
		if (bloodBeforeMeal != null)
		{
			intent.putExtra(ActivityEditorMeal.FIELD_BS_BEFORE_MEAL, bloodBeforeMeal);
		}
		intent.putExtra(ActivityEditorMeal.FIELD_BS_TARGET, bloodTarget);
		intent.putExtra(ActivityEditorMeal.FIELD_INS_INJECTED, insInjected);
		startActivityForResult(intent, createMode ? DIALOG_MEAL_CREATE : DIALOG_MEAL_MODIFY);
	}

	private void showNoteEditor(Versioned<NoteRecord> entity, boolean createMode)
	{
		Log.d("PRFM", "showNoteEditor() started");
		if (createMode)
		{
			NoteRecord rec = new NoteRecord();
			rec.setTime(new Date());
			entity = new Versioned<NoteRecord>(rec);
		}

		Intent intent = new Intent(this, ActivityEditorNote.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);
		startActivityForResult(intent, createMode ? DIALOG_NOTE_CREATE : DIALOG_NOTE_MODIFY);
		Log.d("PRFM", "showNoteEditor() finished");
	}
}