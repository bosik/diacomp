package org.bosik.compensation.face.activities;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import org.bosik.compensation.bo.basic.Versioned;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.bo.diary.records.BloodRecord;
import org.bosik.compensation.bo.diary.records.DiaryRecord;
import org.bosik.compensation.bo.diary.records.InsRecord;
import org.bosik.compensation.bo.diary.records.MealRecord;
import org.bosik.compensation.bo.diary.records.NoteRecord;
import org.bosik.compensation.face.R;
import org.bosik.compensation.face.UIUtils;
import org.bosik.compensation.face.views.DiaryView;
import org.bosik.compensation.face.views.RecordClickListener;
import org.bosik.compensation.persistence.Storage;
import org.bosik.compensation.utils.ErrorHandler;
import org.bosik.compensation.utils.Utils;
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
	// КОНСТАНТЫ
	private static final int		DIALOG_BLOOD_CREATE	= 11;
	private static final int		DIALOG_BLOOD_MODIFY	= 12;
	private static final int		DIALOG_INS_CREATE	= 21;
	private static final int		DIALOG_INS_MODIFY	= 22;
	private static final int		DIALOG_NOTE_CREATE	= 41;
	private static final int		DIALOG_NOTE_MODIFY	= 42;

	private static final int		CONTEXT_ITEM_EDIT	= 0;
	private static final int		CONTEXT_ITEM_REMOVE	= 1;

	// --- отладочная печать ---
	private static final String		TAG					= ActivityDiary.class.getSimpleName();
	// THINK: что произойдёт на смене дат?
	private static Date				curDate				= Calendar.getInstance().getTime();
	private static DiaryPage		curPage				= null;

	// --- форматы ---
	// private static final SimpleDateFormat CaptionFmt = new SimpleDateFormat("d MMMM");
	private final SimpleDateFormat	CaptionFmt			= new SimpleDateFormat("dd.MM.yyyy", Locale.US);

	// КОМПОНЕНТЫ
	private DiaryView				diaryViewLayout;
	private Button					buttonSelectDay;

	// СОБЫТИЯ

	// handled
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		try
		{
			// Log.i(TAG, "DiaryView(): onCreate()");
			setContentView(R.layout.main_diary);

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
				if ((DiaryView.getDownedIndex() < 0) || (DiaryView.getDownedIndex() >= curPage.count()))
				{
					return;
				}

				DiaryRecord rec = curPage.get(DiaryView.getDownedIndex()).getData();
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
	@Override
	public boolean onContextItemSelected(MenuItem item)
	{
		try
		{
			final int ind = DiaryView.getDownedIndex();

			if ((ind < 0) || (ind >= curPage.count()))
			{
				return false;
			}

			// Common options

			switch (item.getItemId())
			{
				case CONTEXT_ITEM_REMOVE:
				{
					diaryViewLayout.getPage().remove(ind);
					postPage(diaryViewLayout.getPage());
					return true;
				}
			}

			// Type-specific options

			Versioned<? extends DiaryRecord> rec = curPage.get(ind);
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
					showMealEditor();
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
	@Override
	public void onRecordClick(int index)
	{
		try
		{
			Versioned<? extends DiaryRecord> rec = curPage.get(index);

			if (rec.getData().getClass() == BloodRecord.class)
			{
				showBloodEditor((Versioned<BloodRecord>) rec, false);
			}
			else if (rec.getData().getClass() == InsRecord.class)
			{
				showInsEditor((Versioned<InsRecord>) rec, false);
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
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent intent)
	{
		super.onActivityResult(requestCode, resultCode, intent);

		try
		{
			switch (requestCode)
			{
				case DIALOG_BLOOD_CREATE:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<BloodRecord> rec = (Versioned<BloodRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						curPage.add(rec);
						postPage(curPage);
					}
					break;
				}

				case DIALOG_BLOOD_MODIFY:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<BloodRecord> rec = (Versioned<BloodRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						curPage.update(rec);
						postPage(curPage);
					}
					break;
				}

				case DIALOG_INS_CREATE:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<InsRecord> rec = (Versioned<InsRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						curPage.add(rec);
						postPage(curPage);
					}
					break;
				}

				case DIALOG_INS_MODIFY:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<InsRecord> rec = (Versioned<InsRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						curPage.update(rec);
						postPage(curPage);
					}
					break;
				}

				case DIALOG_NOTE_CREATE:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<NoteRecord> rec = (Versioned<NoteRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						curPage.add(rec);
						postPage(curPage);
					}
					break;
				}

				case DIALOG_NOTE_MODIFY:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<NoteRecord> rec = (Versioned<NoteRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						curPage.update(rec);
						postPage(curPage);
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
		curPage = Storage.localDiary.getPage(curDate);
		diaryViewLayout.setPage(curPage);
		setCaptionDate(curDate);
	}

	private void postPage(DiaryPage page)
	{
		Storage.localDiary.postPage(page);
		diaryViewLayout.setPage(page);

		// Log.i(TAG, "Posting");
		// Log.v(TAG, "  Date = " + String.valueOf(page.getDate()));
		// Log.v(TAG, "  Stamp = " + String.valueOf(page.getTimeStamp()));
		// Log.d(TAG, "  Page = " + page.writeFull());
	}

	/**
	 * Searches for the last BS record in last scanDaysPeriod days
	 * 
	 * @param scanDaysPeriod
	 * @return BS record if found, null otherwise
	 */
	private BloodRecord lastBlood(int scanDaysPeriod)
	{
		// TODO: make use of getPages(), not getPage()
		// TODO: move this away from UI

		Date d = Utils.now();

		for (int i = 1; i <= scanDaysPeriod; i++)
		{
			DiaryPage page = Storage.localDiary.getPage(d);
			if (page != null)
			{
				for (int j = page.count() - 1; j >= 0; j--)
				{
					if (page.get(j).getData().getClass() == BloodRecord.class)
					{
						return (BloodRecord) page.get(j).getData();
					}
				}
			}
			d = Utils.getPrevDay(d);
		}

		return null;
	}

	private void showBloodEditor(Versioned<BloodRecord> entity, boolean createMode)
	{
		if (createMode)
		{
			// FIXME: hardcoded scan period
			final int SCAN_DAYS = 5;
			BloodRecord prev = lastBlood(SCAN_DAYS);
			BloodRecord rec = new BloodRecord();
			rec.setTime(Utils.curMinutes());
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
			rec.setTime(Utils.curMinutes());
			entity = new Versioned<InsRecord>(rec);
		}

		Intent intent = new Intent(this, ActivityEditorIns.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);

		startActivityForResult(intent, createMode ? DIALOG_INS_CREATE : DIALOG_INS_MODIFY);
	}

	private void showMealEditor()
	{
		// TODO: make it actual editor intent
		startActivity(new Intent(this, ActivityMeal.class));
	}

	private void showNoteEditor(Versioned<NoteRecord> entity, boolean createMode)
	{
		if (createMode)
		{
			NoteRecord rec = new NoteRecord();
			rec.setTime(Utils.curMinutes());
			entity = new Versioned<NoteRecord>(rec);
		}

		Intent intent = new Intent(this, ActivityEditorNote.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);
		startActivityForResult(intent, createMode ? DIALOG_NOTE_CREATE : DIALOG_NOTE_MODIFY);
	}
}