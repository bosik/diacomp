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
package org.bosik.diacomp.android.frontend.fragments;

import java.util.Arrays;
import java.util.Date;
import java.util.Locale;
import java.util.Timer;
import java.util.TimerTask;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.AccountUtils;
import org.bosik.diacomp.android.backend.common.db.tables.TableDiary;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.backend.features.sync.ServerTimeService;
import org.bosik.diacomp.android.backend.features.sync.TimeServiceInternal;
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
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.PostprandUtils;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.TimeUtils;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import android.app.Activity;
import android.content.Intent;
import android.database.ContentObserver;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;

public class FragmentTabDiary extends Fragment
{
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

	/**
	 * Time error triggering time zone warning, in minutes
	 */
	private static final int		LIMIT_TIMEZONE				= 30;
	/**
	 * Time error triggering offset warning, in minutes
	 */
	private static final int		LIMIT_OFFSET				= 10;

	// Services
	private DiaryService			diary;
	private PreferencesTypedService	preferences;

	// Widgets
	TextView						textWarningTime;
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
		getActivity().getContentResolver().registerContentObserver(TableDiary.CONTENT_URI, true, observer);
	}

	@Override
	public void onDestroy()
	{
		super.onDestroy();
		getActivity().getContentResolver().unregisterContentObserver(observer);
	}

	@Override
	public void onResume()
	{
		super.onResume();

		class DatePair
		{
			public Date	localTime;
			public Date	serverTime;

			public DatePair(Date localTime, Date serverTime)
			{
				this.localTime = localTime;
				this.serverTime = serverTime;
			}
		}

		new Timer().schedule(new TimerTask()
		{
			@Override
			public void run()
			{
				new AsyncTask<Void, Void, DatePair>()
				{
					@Override
					protected DatePair doInBackground(Void... arg0)
					{
						FragmentActivity context = FragmentTabDiary.this.getActivity();
						if (context != null)
						{
							ServerTimeService service = TimeServiceInternal.getInstance(context);
							Date serverTime = service.getServerTime(true);
							return new DatePair(new Date(), serverTime);
						}
						else
						{
							// activity was detached
							return new DatePair(null, null);
						}
					}

					@Override
					protected void onPostExecute(DatePair pair)
					{
						if (pair.serverTime != null)
						{
							Date serverTime = pair.serverTime;
							Date localTime = pair.localTime;

							Long offset = TimeUtils.guessTimeZoneOffset(serverTime, localTime);
							if (offset != null)
							{
								int errorMin = (int) ((localTime.getTime() - serverTime.getTime()) / Utils.MsecPerMin);
								int absErrorMin = Math.abs(errorMin);
								if (absErrorMin > LIMIT_TIMEZONE)
								{
									long offsetMin = offset / Utils.MsecPerMin;
									String timeZone = String.format(Locale.US, "%+02d:%02d",
											offsetMin / Utils.MinPerHour, offsetMin % Utils.MinPerHour);
									String msg = getActivity().getString(R.string.warning_time_zone, timeZone);
									textWarningTime.setText(msg);
									textWarningTime.setVisibility(View.VISIBLE);
								}
								else
								{
									if (absErrorMin > LIMIT_OFFSET)
									{
										String s0, s1, s2;
										if (errorMin > 0)
										{
											s0 = getString(R.string.warning_time_delay_ahead_0);
											s1 = getString(R.string.warning_time_delay_ahead_1);
											s2 = getString(R.string.warning_time_delay_ahead_2);
										}
										else
										{
											s0 = getString(R.string.warning_time_delay_behind_0);
											s1 = getString(R.string.warning_time_delay_behind_1);
											s2 = getString(R.string.warning_time_delay_behind_2);
										}

										String f = Utils.getNumberName(absErrorMin, s0, s1, s2);

										textWarningTime.setText(String.format(f, absErrorMin));
										textWarningTime.setVisibility(View.VISIBLE);
									}
									else
									{
										textWarningTime.setVisibility(View.GONE);
									}
								}
							}
						}
					}
				}.execute();
			}
		}, 1000);
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		setHasOptionsMenu(true);

		// services
		diary = new DiaryLocalService(getActivity());
		preferences = new PreferencesTypedService(new PreferencesLocalService(getActivity()));

		// Widgets binding
		View rootView = inflater.inflate(R.layout.fragment_tab_diary, container, false);

		list = (DiaryDayView) rootView.findViewById(R.id.listDiaryDay);
		buttonAddBlood = (Button) rootView.findViewById(R.id.buttonAddBlood);
		buttonAddIns = (Button) rootView.findViewById(R.id.buttonAddIns);
		buttonAddMeal = (Button) rootView.findViewById(R.id.buttonAddMeal);
		buttonAddNote = (Button) rootView.findViewById(R.id.buttonAddNote);
		textWarningTime = (TextView) rootView.findViewById(R.id.textWarningTime);

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

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
	{
		super.onCreateOptionsMenu(menu, inflater);
		inflater.inflate(R.menu.actions_diary, menu);

		if (AccountUtils.getAccounts(getActivity()).length > 0)
		{
			MenuItem item = menu.findItem(R.id.item_common_login);
			if (item != null)
			{
				item.setVisible(false);
			}
		}
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
			intent.putExtra(ActivityEditor.FIELD_CREATE_MODE, createMode);

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
			intent.putExtra(ActivityEditor.FIELD_CREATE_MODE, createMode);

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

			BloodRecord bloodBase = PostprandUtils.findLastBlood(diary, entity.getData().getTime(),
					SCAN_FOR_BLOOD_BEFORE_MEAL, true);
			BloodRecord bloodLast = PostprandUtils.findLastBlood(diary, entity.getData().getTime(),
					SCAN_FOR_BLOOD_BEFORE_MEAL, false);
			Double bloodBaseValue = bloodBase == null ? null : bloodBase.getValue();
			Double bloodLastValue = bloodLast == null ? null : bloodLast.getValue();
			Double bloodTarget = preferences.getDoubleValue(PreferenceID.TARGET_BS);
			InsRecord insRecord = PostprandUtils.findNearestInsulin(diary, entity.getData().getTime(),
					SCAN_FOR_INS_AROUND_MEAL);
			Double insInjected = insRecord == null ? null : insRecord.getValue();

			Intent intent = new Intent(getActivity(), ActivityEditorMeal.class);
			intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
			intent.putExtra(ActivityEditor.FIELD_CREATE_MODE, createMode);
			if (bloodBaseValue != null)
			{
				intent.putExtra(ActivityEditorMeal.FIELD_BS_BASE, bloodBaseValue);
			}
			if (bloodLastValue != null)
			{
				intent.putExtra(ActivityEditorMeal.FIELD_BS_LAST, bloodLastValue);
			}

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
			if (createMode)
			{
				NoteRecord rec = new NoteRecord();
				rec.setTime(new Date());
				entity = new Versioned<NoteRecord>(rec);
			}

			Intent intent = new Intent(getActivity(), ActivityEditorNote.class);
			intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
			intent.putExtra(ActivityEditor.FIELD_CREATE_MODE, createMode);
			startActivityForResult(intent, createMode ? DIALOG_NOTE_CREATE : DIALOG_NOTE_MODIFY);
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
					// case DIALOG_MEAL_CREATE:
				case DIALOG_NOTE_CREATE:

				case DIALOG_BLOOD_MODIFY:
				case DIALOG_INS_MODIFY:
					// case DIALOG_MEAL_MODIFY:
				case DIALOG_NOTE_MODIFY:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<DiaryRecord> rec = (Versioned<DiaryRecord>) intent.getExtras()
								.getSerializable(ActivityEditor.FIELD_ENTITY);

						diary.save(Arrays.asList(rec));

						// do it manually in case observer is broken
						list.refresh();
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
