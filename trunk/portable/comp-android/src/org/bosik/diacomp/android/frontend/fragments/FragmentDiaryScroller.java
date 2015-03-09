package org.bosik.diacomp.android.frontend.fragments;

import java.util.Arrays;
import java.util.Date;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.preferences.PreferencesLocalService;
import org.bosik.diacomp.android.frontend.activities.ActivityEditor;
import org.bosik.diacomp.android.frontend.activities.ActivityEditorBlood;
import org.bosik.diacomp.android.frontend.activities.ActivityEditorIns;
import org.bosik.diacomp.android.frontend.activities.ActivityEditorMeal;
import org.bosik.diacomp.android.frontend.activities.ActivityEditorNote;
import org.bosik.diacomp.android.frontend.views.diary.DiaryDayView;
import org.bosik.diacomp.android.frontend.views.diary.DiaryDayView.OnRecordClickListener;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.PostprandUtils;
import org.bosik.diacomp.core.services.preferences.Preference;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.Utils;
import android.app.Activity;
import android.content.Intent;
import android.database.ContentObserver;
import android.net.Uri;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;

public class FragmentDiaryScroller extends Fragment
{
	private static final String		TAG							= FragmentDiaryScroller.class.getSimpleName();

	// Constants
	private static final int		DIALOG_BLOOD_CREATE			= 11;
	private static final int		DIALOG_BLOOD_MODIFY			= 12;
	private static final int		DIALOG_INS_CREATE			= 21;
	private static final int		DIALOG_INS_MODIFY			= 22;
	private static final int		DIALOG_MEAL_CREATE			= 31;
	private static final int		DIALOG_MEAL_MODIFY			= 32;
	private static final int		DIALOG_NOTE_CREATE			= 41;
	private static final int		DIALOG_NOTE_MODIFY			= 42;

	// FIXME: hardcoded patient params
	private static final long		SCAN_FOR_BLOOD_FINGER		= 5 * Utils.SecPerDay;
	private static final int		SCAN_FOR_BLOOD_BEFORE_MEAL	= 4 * Utils.SecPerHour;
	private static final long		SCAN_FOR_INS_AROUND_MEAL	= 3 * Utils.SecPerHour;

	// Services
	private DiaryService			diary;
	private PreferencesTypedService	preferences;

	// Widgets
	DiaryDayView					list;
	private Button					buttonAddBlood;
	private Button					buttonAddIns;
	private Button					buttonAddMeal;
	private Button					buttonAddNote;

	private ContentObserver			observer					= new ContentObserver(null)
																{
																	@Override
																	public void onChange(boolean selfChange)
																	{
																		this.onChange(selfChange, null);
																	}

																	@Override
																	public void onChange(boolean selfChange, Uri uri)
																	{
																		if (list != null)
																		{
																			list.refresh();
																		}
																	}
																};

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		getActivity().getContentResolver().registerContentObserver(DiaryContentProvider.CONTENT_DIARY_URI, true,
				observer);
	};

	@Override
	public void onDestroy()
	{
		super.onDestroy();
		getActivity().getContentResolver().unregisterContentObserver(observer);
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		setHasOptionsMenu(true);

		// services
		diary = new DiaryLocalService(getActivity().getContentResolver());
		preferences = new PreferencesLocalService(getActivity().getContentResolver());

		// Widgets binding
		View rootView = inflater.inflate(R.layout.fragment_diary_scroller, container, false);

		list = (DiaryDayView) rootView.findViewById(R.id.listDiaryDay);
		buttonAddBlood = (Button) rootView.findViewById(R.id.buttonAddBlood);
		buttonAddIns = (Button) rootView.findViewById(R.id.buttonAddIns);
		buttonAddMeal = (Button) rootView.findViewById(R.id.buttonAddMeal);
		buttonAddNote = (Button) rootView.findViewById(R.id.buttonAddNote);

		// Events
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

		list.setOnRecordClickListener(new OnRecordClickListener()
		{
			@Override
			public void onRecordClick(Versioned<DiaryRecord> record)
			{
				if (record.getData() instanceof BloodRecord)
				{
					showBloodEditor(new Versioned<BloodRecord>(record), false);
				}
				else if (record.getData() instanceof InsRecord)
				{
					showInsEditor(new Versioned<InsRecord>(record), false);
				}
				else if (record.getData() instanceof MealRecord)
				{
					showMealEditor(new Versioned<MealRecord>(record), false);
				}
				else if (record.getData() instanceof NoteRecord)
				{
					showNoteEditor(new Versioned<NoteRecord>(record), false);
				}
			}
		});

		return rootView;
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

			Intent intent = new Intent(getActivity(), ActivityEditorBlood.class);
			intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
			intent.putExtra(ActivityEditor.FIELD_MODE, createMode);

			startActivityForResult(intent, createMode ? DIALOG_BLOOD_CREATE : DIALOG_BLOOD_MODIFY);
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, getActivity());
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

			Intent intent = new Intent(getActivity(), ActivityEditorIns.class);
			intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
			intent.putExtra(ActivityEditor.FIELD_MODE, createMode);

			startActivityForResult(intent, createMode ? DIALOG_INS_CREATE : DIALOG_INS_MODIFY);
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, getActivity());
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

			Intent intent = new Intent(getActivity(), ActivityEditorMeal.class);
			intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
			intent.putExtra(ActivityEditor.FIELD_MODE, createMode);
			if (bloodBeforeMeal != null)
			{
				intent.putExtra(ActivityEditorMeal.FIELD_BS_BEFORE_MEAL, bloodBeforeMeal);
			}

			Double bloodTarget = preferences.getDoubleValue(Preference.TARGET_BS);

			intent.putExtra(ActivityEditorMeal.FIELD_BS_TARGET, bloodTarget);
			intent.putExtra(ActivityEditorMeal.FIELD_INS_INJECTED, insInjected);
			startActivityForResult(intent, createMode ? DIALOG_MEAL_CREATE : DIALOG_MEAL_MODIFY);
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, getActivity());
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

			Intent intent = new Intent(getActivity(), ActivityEditorNote.class);
			intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
			intent.putExtra(ActivityEditor.FIELD_MODE, createMode);
			startActivityForResult(intent, createMode ? DIALOG_NOTE_CREATE : DIALOG_NOTE_MODIFY);
			Log.d("PRFM", "showNoteEditor() finished");
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, getActivity());
		}
	}

	// handled
	@SuppressWarnings("unchecked")
	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent intent)
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
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<DiaryRecord> rec = (Versioned<DiaryRecord>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);

						diary.save(Arrays.<Versioned<DiaryRecord>> asList(rec));
						Storage.syncDiary(rec.getId());
					}
					break;
				}
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, getActivity());
		}
	}
}