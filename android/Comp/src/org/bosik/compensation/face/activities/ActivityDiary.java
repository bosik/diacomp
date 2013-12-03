package org.bosik.compensation.face.activities;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
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
	private static final int		DIALOG_BLOOD_CREATE	= 1;
	private static final int		DIALOG_BLOOD_MODIFY	= 2;
	private static final int		DIALOG_NOTE_CREATE	= 3;
	private static final int		DIALOG_NOTE_MODIFY	= 4;

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
			setContentView(R.layout.diary);

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
			 * if (intent.hasExtra("date")) { // TODO: опасно... а вдруг в Intent мусор? // TODO:
			 * константы Date date = (Date) intent.getSerializableExtra("date"); openPage(date);
			 * Log.d(TAG, "DiaryView(): onCreate(), date is specified: " + date); } else
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

				String capt = "Unsupported type";
				DiaryRecord rec = curPage.get(DiaryView.getDownedIndex());
				Class<? extends DiaryRecord> c = rec.getClass();
				if (c == BloodRecord.class)
				{
					capt = getString(R.string.common_rectype_blood);
				}
				else if (c == InsRecord.class)
				{
					capt = getString(R.string.common_rectype_ins);
				}
				else if (c == MealRecord.class)
				{
					capt = getString(R.string.common_rectype_meal);
				}
				else if (c == NoteRecord.class)
				{
					capt = getString(R.string.common_rectype_note);
				}

				menu.setHeaderTitle(capt);
				String[] menuItems = getResources().getStringArray(R.array.context_edit);
				for (int i = 0; i < menuItems.length; i++)
				{
					menu.add(Menu.NONE, i, i, menuItems[i]);
				}

				// menu.getItem(0).setCheckable(true);
				// menu.getItem(0).setChecked(true);
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
					// TODO: handler
					UIUtils.showTip(this, "Creating ins is not implemented yet");
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
			/*
			 * AdapterContextMenuInfo info = (AdapterContextMenuInfo) item.getMenuInfo(); int
			 * menuItemIndex = item.getItemId(); String[] menuItems =
			 * getResources().getStringArray(R.array.context_edit); String menuItemName =
			 * menuItems[menuItemIndex];
			 */

			if ((DiaryView.getDownedIndex() < 0) || (DiaryView.getDownedIndex() >= curPage.count()))
			{
				return false;
			}

			DiaryRecord rec = curPage.get(DiaryView.getDownedIndex());
			Class<? extends DiaryRecord> c = rec.getClass();

			if (c == BloodRecord.class)
			{
				switch (item.getItemId())
				{
				// TODO: rename, use constants

				// изменить
					case 0:
					{
						BloodRecord temp = (BloodRecord) rec;
						// TODO: не использовать indexOnEditing, а только
						// diaryViewLayout.clickedIndex?
						// indexOnEditing = diaryViewLayout.downedIndex;
						showBloodEditor(temp, false);
						break;
					}
					// удалить
					case 1:
					{
						// TODO: implement removing
						// UIUtils.showTip(this, "Removing blood is not implemented yet");
						diaryViewLayout.getPage().remove(DiaryView.getDownedIndex());
						postPage(diaryViewLayout.getPage());

						break;
					}
				}
			}

			else

			if (c == InsRecord.class)
			{
				switch (item.getItemId())
				{
				// TODO: rename, use constants

				// изменить
					case 0:
					{
						InsRecord temp = (InsRecord) rec;
						// TODO: не использовать indexOnEditing, а только
						// diaryViewLayout.clickedIndex?
						// indexOnEditing = diaryViewLayout.downedIndex;
						// showBloodEditor(temp.getTime(), temp.getValue(), temp.getFinger(),
						// false);
						UIUtils.showTip(this, "Editing ins is not implemented yet");
						break;
					}
					// удалить
					case 1:
					{
						// TODO: implement removing
						UIUtils.showTip(this, "Removing ins is not implemented yet");
						break;
					}
				}
			}

			// TODO: implement other record types
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
				case R.id.buttonAddMeal:
				{
					// showNoteEditor(0, "", true);
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
			DiaryRecord rec = curPage.get(index);

			if (rec.getClass() == BloodRecord.class)
			{
				showBloodEditor((BloodRecord) rec, false);
			}
			else if (rec.getClass() == NoteRecord.class)
			{
				showNoteEditor((NoteRecord) rec, false);
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
						BloodRecord rec = (BloodRecord) intent.getExtras().getSerializable(ActivityEditor.FIELD_ENTITY);
						curPage.add(rec);
						postPage(curPage);
					}
					break;
				}

				case DIALOG_BLOOD_MODIFY:
				{
					if (resultCode == RESULT_OK)
					{
						BloodRecord rec = (BloodRecord) intent.getExtras().getSerializable(ActivityEditor.FIELD_ENTITY);
						curPage.update(rec);
						postPage(curPage);
					}
					break;
				}

				case DIALOG_NOTE_CREATE:
				{
					if (resultCode == RESULT_OK)
					{
						NoteRecord rec = (NoteRecord) intent.getExtras().getSerializable(ActivityEditor.FIELD_ENTITY);
						curPage.add(rec);
						postPage(curPage);
					}
					break;
				}

				case DIALOG_NOTE_MODIFY:
				{
					if (resultCode == RESULT_OK)
					{
						NoteRecord rec = (NoteRecord) intent.getExtras().getSerializable(ActivityEditor.FIELD_ENTITY);
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
					if (page.get(j).getClass() == BloodRecord.class)
					{
						return (BloodRecord) page.get(j);
					}
				}
			}
			d = Utils.getPrevDay(d);
		}

		return null;
	}

	private void showBloodEditor(BloodRecord entity, boolean createMode)
	{
		if (createMode)
		{
			// FIXME: hardcoded scan period
			final int SCAN_DAYS = 5;
			BloodRecord prev = lastBlood(SCAN_DAYS);
			entity = new BloodRecord();
			entity.setTime(Utils.curMinutes());
			entity.setFinger(((prev == null) || (prev.getFinger() == -1)) ? -1 : ((prev.getFinger() + 1) % 10));
		}

		Intent intent = new Intent(this, ActivityEditorBlood.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);

		startActivityForResult(intent, createMode ? DIALOG_BLOOD_CREATE : DIALOG_BLOOD_MODIFY);
	}

	private void showMealEditor()
	{
		// TODO: make it actual editor intent
		startActivity(new Intent(this, ActivityMeal.class));
	}

	private void showNoteEditor(NoteRecord entity, boolean createMode)
	{
		if (createMode)
		{
			entity = new NoteRecord();
			entity.setTime(Utils.curMinutes());
		}

		Intent intent = new Intent(this, ActivityEditorNote.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);
		startActivityForResult(intent, createMode ? DIALOG_NOTE_CREATE : DIALOG_NOTE_MODIFY);
	}
}