package org.bosik.diacomp.android.frontend.activities;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
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
import org.bosik.diacomp.core.services.diary.PostprandUtils;
import org.bosik.diacomp.core.utils.Utils;
import android.app.Activity;
import android.content.Intent;
import android.graphics.Color;
import android.os.AsyncTask;
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
import android.widget.ScrollView;

public class ActivityDiary extends Activity
{
	private static final String			TAG							= ActivityDiary.class.getSimpleName();

	public static final String			KEY_DATE					= "diacomp.activitydiary.date";

	private static final int			DIALOG_BLOOD_CREATE			= 11;
	private static final int			DIALOG_BLOOD_MODIFY			= 12;
	private static final int			DIALOG_INS_CREATE			= 21;
	private static final int			DIALOG_INS_MODIFY			= 22;
	private static final int			DIALOG_MEAL_CREATE			= 31;
	private static final int			DIALOG_MEAL_MODIFY			= 32;
	private static final int			DIALOG_NOTE_CREATE			= 41;
	private static final int			DIALOG_NOTE_MODIFY			= 42;

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

	private Button						buttonAddBlood;
	private Button						buttonAddIns;
	private Button						buttonAddMeal;
	private Button						buttonAddNote;

	private ScrollView					scrollView;

	// handled
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		try
		{
			setContentView(R.layout.activity_diary);

			// services
			diary = new DiaryLocalService(getContentResolver());

			// UI: components

			diaryViewLayout = (DiaryView) findViewById(R.id.diaryViewLayout);
			registerForContextMenu(diaryViewLayout);
			buttonPrevDay = (Button) findViewById(R.id.buttonPrevDay);
			buttonNextDay = (Button) findViewById(R.id.buttonNextDay);
			buttonSelectDay = (Button) findViewById(R.id.buttonSelectDay);

			buttonAddBlood = (Button) findViewById(R.id.buttonAddBlood);
			buttonAddIns = (Button) findViewById(R.id.buttonAddIns);
			buttonAddMeal = (Button) findViewById(R.id.buttonAddMeal);
			buttonAddNote = (Button) findViewById(R.id.buttonAddNote);

			scrollView = (ScrollView) findViewById(R.id.scrollView);

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

			buttonAddBlood.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					showBloodEditor(null, true);
				}
			});
			buttonAddIns.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					showInsEditor(null, true);
				}
			});
			buttonAddMeal.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					showMealEditor(null, true);
				}
			});
			buttonAddNote.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					showNoteEditor(null, true);
				}
			});

			scrollView.setBackgroundColor(Color.WHITE);
			diaryViewLayout.setOnRecordClickListener(new RecordClickListener()
			{
				// handled
				@SuppressWarnings("unchecked")
				@Override
				public void onRecordClick(int index)
				{
					try
					{
						Versioned<? extends DiaryRecord> rec = curRecords.get(index);

						if (rec.getData() instanceof BloodRecord)
						{
							showBloodEditor((Versioned<BloodRecord>) rec, false);
						}
						else if (rec.getData() instanceof InsRecord)
						{
							showInsEditor((Versioned<InsRecord>) rec, false);
						}
						else if (rec.getData() instanceof MealRecord)
						{
							showMealEditor((Versioned<MealRecord>) rec, false);
						}
						else if (rec.getData() instanceof NoteRecord)
						{
							showNoteEditor((Versioned<NoteRecord>) rec, false);
						}
					}
					catch (Exception e)
					{
						ErrorHandler.handle(e, ActivityDiary.this);
					}
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
					showMealEditor(null, true);
					return true;
				}
				case R.id.item_diary_addnote:
				{
					showNoteEditor(null, true);
					return true;
				}
				case R.id.item_diary_preferences:
				{
					Intent settingsActivity = new Intent(getBaseContext(), ActivityPreferences.class);
					startActivity(settingsActivity);
					return true;
				}
				case R.id.item_diary_sync:
				{
					new AsyncTask<Void, Void, Map<String, Integer>>()
					{
						final String	DIARY	= "diary";
						final String	FOOD	= "food";
						final String	DISH	= "dish";

						@Override
						protected Map<String, Integer> doInBackground(Void... arg0)
						{
							Map<String, Integer> result = new HashMap<String, Integer>();

							result.put(DIARY, Storage.syncDiary());
							result.put(FOOD, Storage.syncFoodbase());
							result.put(DISH, Storage.syncDishbase());

							return result;
						}

						@Override
						protected void onPostExecute(Map<String, Integer> result)
						{
							String message = "";

							Integer countDiary = result.get(DIARY);
							if (countDiary == null)
							{
								message += "Diary: failed\n";
							}
							else if (countDiary > 0)
							{
								message += String.format("Diary: %d\n", countDiary);
							}
							else
							{
								message += "Diary: \t\tno changes\n";
							}

							// =================================================================

							Integer countFood = result.get(FOOD);
							if (countFood == null)
							{
								message += "Foods: failed\n";
							}
							else if (countFood > 0)
							{
								message += String.format("Foods: %d\n", countFood);
							}
							else
							{
								message += "Foods: \tno changes\n";
							}

							// =================================================================

							Integer countDish = result.get(DISH);
							if (countDish == null)
							{
								message += "Dishes: failed";
							}
							else if (countDish > 0)
							{
								message += String.format("Dishes: %d", countDish);
							}
							else
							{
								message += "Dishes:\tno changes";
							}

							UIUtils.showTip(ActivityDiary.this, message);
						}
					}.execute();

					return true;
				}

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
					String id = curRecords.get(ind).getId();
					diary.delete(id);
					openPage(curDate);

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

				case DIALOG_BLOOD_MODIFY:
				case DIALOG_INS_MODIFY:
				case DIALOG_MEAL_MODIFY:
				case DIALOG_NOTE_MODIFY:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<DiaryRecord> rec = (Versioned<DiaryRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);

						diary.save(Arrays.<Versioned<DiaryRecord>> asList(rec));
						openPage(curDate);
						Storage.syncDiary(rec.getId());
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
			UIUtils.showTip(this, "Diary verification failed");
		}

		diaryViewLayout.setRecords(curRecords);
		buttonSelectDay.setText(FORMAT_DATE.format(curDate));
	}

	// handled
	void showBloodEditor(Versioned<BloodRecord> entity, boolean createMode)
	{
		try
		{
			if (createMode)
			{
				BloodRecord prev = PostprandUtils.findLastBlood(diary, new Date(), SCAN_FOR_BLOOD_FINGER, false);
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
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// handled
	void showInsEditor(Versioned<InsRecord> entity, boolean createMode)
	{
		try
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
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// handled
	void showMealEditor(Versioned<MealRecord> entity, boolean createMode)
	{
		try
		{
			if (createMode)
			{
				MealRecord rec = new MealRecord();
				rec.setTime(new Date());
				entity = new Versioned<MealRecord>(rec);
			}

			BloodRecord prevBlood = PostprandUtils.findLastBlood(diary, entity.getData().getTime(),
					SCAN_FOR_BLOOD_BEFORE_MEAL, false);
			Double bloodBeforeMeal = prevBlood == null ? null : prevBlood.getValue();
			InsRecord insRecord = PostprandUtils.findNearestInsulin(diary, entity.getData().getTime(),
					SCAN_FOR_INS_AROUND_MEAL);
			Double insInjected = insRecord == null || (prevBlood != null && prevBlood.isPostPrand()) ? null : insRecord
					.getValue();

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
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// handled
	void showNoteEditor(Versioned<NoteRecord> entity, boolean createMode)
	{
		try
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
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}
}