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
package org.bosik.diacomp.android.frontend.activities;

import android.graphics.Color;
import android.graphics.Typeface;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.analyze.KoofServiceInternal;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView.OnChangeListener;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.analyze.RateService;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;

public class ActivityEditorMeal extends ActivityEditorTime<MealRecord>
{
	public static final String TAG = ActivityEditorMeal.class.getSimpleName();

	public static final String FIELD_BS_BASE      = "bosik.pack.bs.base";
	public static final String FIELD_BS_LAST      = "bosik.pack.bs.last";
	public static final String FIELD_BS_TARGET    = "bosik.pack.bs.target";
	public static final String FIELD_INS_INJECTED = "bosik.pack.insInjected";

	// FIXME: hardcoded BS value
	private static final double BS_HYPOGLYCEMIA  = 3.7;
	private static final double CARB_COLOR_LIMIT = 1.5;

	private DiaryService diary;

	// components
	private Button         buttonTime;
	private Button         buttonDate;
	private TextView       textMealInsulinCorrection;
	private TextView       textMealInsulinResult;
	private TextView       textMealCarbsCorrection;
	private TextView       textMealCarbsResult;
	private TextView       textMealExpectedBs;
	private MealEditorView mealEditor;

	// localization
	private String captionDose;
	private String captionGramm;
	private String captionMmol;

	// ======================================================================================================

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_meal_3);

		captionDose = getString(R.string.common_unit_insulin);
		captionGramm = getString(R.string.common_unit_mass_gramm);
		captionMmol = getString(R.string.common_unit_bs_mmoll);

		// components

		buttonTime = (Button) findViewById(R.id.buttonMealTime);
		buttonTime.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showTimePickerDialog();
			}
		});
		buttonDate = (Button) findViewById(R.id.buttonMealDate);
		buttonDate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showDatePickerDialog();
			}
		});

		textMealInsulinCorrection = (TextView) findViewById(R.id.textMealInsulinCorrection);
		textMealInsulinResult = (TextView) findViewById(R.id.textMealInsulinResult);
		textMealCarbsCorrection = (TextView) findViewById(R.id.textMealCarbsCorrection);
		textMealCarbsResult = (TextView) findViewById(R.id.textMealCarbsResult);
		textMealExpectedBs = (TextView) findViewById(R.id.textMealExpectedBs);

		mealEditor = (MealEditorView) findViewById(R.id.mealEditorMeal);

		// list.addHeaderView(findViewById(R.id.layoutMealParent));

		// setting listeners
		mealEditor.setOnChangeListener(new OnChangeListener()
		{
			@Override
			public void onChange(List<FoodMassed> items)
			{
				entity.getData().clear();
				for (FoodMassed item : mealEditor.getData())
				{
					entity.getData().add(item);
				}

				modified();
				showMealInfo();
			}
		});
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		try
		{
			getMenuInflater().inflate(R.menu.actions_meal, menu);
			MenuItem itemShort = menu.findItem(R.id.item_meal_short);
			itemShort.setChecked(entity.getData().getShortMeal());
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		final MealRecord data = entity.getData();

		switch (item.getItemId())
		{
			case R.id.item_meal_short:
			{
				data.setShortMeal(!data.getShortMeal());
				item.setChecked(data.getShortMeal());
				modified();
				return true;
			}
			case R.id.item_meal_info:
			{
				String s = getString(R.string.editor_meal_tip_meal_info);
				String info = String
						.format(Locale.US, s, data.getProts(), data.getFats(), data.getCarbs(), data.getCarbs() / Utils.CARB_PER_BU,
								data.getValue(), data.getMass());
				UIUtils.showLongTip(ActivityEditorMeal.this, info);
				return true;
			}
			default:
			{
				return false;
			}
		}
	}

	private void showMealContent()
	{
		List<FoodMassed> items = new ArrayList<>();

		for (int i = 0; i < entity.getData().count(); i++)
		{
			items.add(entity.getData().get(i));
		}

		mealEditor.setData(items);
	}

	private void showMealInfo()
	{
		// | Base | Last | Mod | BaseBS |
		// |------|------|-----|--------|
		// | ---- | ---- | Std | target | Dosage = carbs * k / q
		// | ---- | hypo | Hyp | lastBS | Delta_carbs = (BS_target - BS_last) / k <------------
		// | ---- | belw | Std | target | Dosage = carbs * k / q
		// | ---- | abov | Std | target | Dosage = carbs * k / q
		// | hypo | ---- | Std | baseBS |
		// | hypo | hypo | Hyp | lastBS | Delta_carbs = (BS_target - BS_last) / k <------------
		// | hypo | belw | Std | baseBS |
		// | hypo | abov | Std | baseBS |
		// | belw | ---- | Std | baseBS |
		// | belw | hypo | Hyp | lastBS | Delta_carbs = (BS_target - BS_last) / k <------------
		// | belw | belw | Std | baseBS |
		// | belw | abov | Std | baseBS |
		// | abov | ---- | Std | baseBS |
		// | abov | hypo | Hyp | lastBS | Delta_carbs = (BS_target - BS_last) / k <------------
		// | abov | belw | Std | baseBS |
		// | abov | abov | Std | baseBS |

		// Legend:
		// B - base BS
		// L - last BS
		// M - mode
		// - - no BS
		// h - hypoglycemic BS (0..BS_HYPOGLYCEMIA)
		// b - BS below target (BS_HYPOGLYCEMIA..BS_TARGET)
		// a - BS above target (BS_TARGET..)

		// commons
		int minutesTime = Utils.getDayMinutesUTC(entity.getData().getTime());
		RateService rateService = KoofServiceInternal.getInstance(this);
		Koof koof = rateService.getKoof(minutesTime);
		double carbs = entity.getData().getCarbs();
		double prots = entity.getData().getProts();

		Double bsBase = getBsBase();
		Double bsLast = getBsLast();
		Double bsTarget = getBsTarget();
		double insInjected = getInsInjected();

		//		if (bsLast != null && bsLast < BS_HYPOGLYCEMIA && (bsBase == null || Math.abs(bsBase - bsLast) > Utils.EPS))
		//		{
		//			if (koof != null)
		//			{
		//				double shiftedCarbs = (prots * koof.getP() + (bsTarget - bsLast)) / koof.getK() - carbs;
		//				showCarbsCorrection(shiftedCarbs);
		//			}
		//			else
		//			{
		//				showCarbsCorrection(null);
		//			}
		//
		//			showInsulinCorrection(bsBase, bsTarget, carbs, prots, koof);
		//			showExpectedBs(bsLast, 0, carbs, prots, koof);
		//		}
		//		else
		{
			showInsulinCorrection(bsBase, bsTarget, carbs, prots, koof);

			if (koof != null)
			{
				double deltaBS = (bsBase == null ? 0.0 : bsTarget - bsBase);
				double shiftedCarbs = (insInjected * koof.getQ() - prots * koof.getP() + deltaBS) / koof.getK() - carbs;
				showCarbsCorrection(shiftedCarbs, insInjected);
			}
			else
			{
				showCarbsCorrection(null, 0);
			}

			showExpectedBs(bsBase, insInjected, carbs, prots, koof);
		}
	}

	private void showInsulinCorrection(Double inputBS, Double targetBS, double carbs, double prots, Koof koof)
	{
		if (targetBS != null && koof != null && koof.getQ() > Utils.EPS)
		{
			double deltaBS = (inputBS == null ? 0.0 : targetBS - inputBS);
			double dosageRequired = (-deltaBS + carbs * koof.getK() + prots * koof.getP()) / koof.getQ();

			double dosageCorrection = dosageRequired - getInsInjected();
			textMealInsulinCorrection.setText(String.format(Locale.US, "%+.1f %s", dosageCorrection, captionDose));
			if (dosageCorrection > 0)
			{
				textMealInsulinCorrection.setTypeface(Typeface.DEFAULT_BOLD);
			}
			else
			{
				textMealInsulinCorrection.setTypeface(Typeface.DEFAULT);
			}

			if (dosageRequired > 0)
			{
				textMealInsulinResult.setText(String.format(Locale.US, "(%.1f %s)", dosageRequired, captionDose));
			}
			else
			{
				textMealInsulinResult.setText("");
			}
		}
		else
		{
			textMealInsulinCorrection.setTypeface(Typeface.DEFAULT);
			textMealInsulinCorrection.setText("?");
			textMealInsulinResult.setText("");
		}
	}

	private void showCarbsCorrection(Double shiftedCarbs, double insInjected)
	{
		if (shiftedCarbs != null)
		{
			textMealCarbsCorrection.setText(String.format(Locale.US, "%+.0f %s", shiftedCarbs, captionGramm));

			if (shiftedCarbs < -CARB_COLOR_LIMIT)
			{
				textMealCarbsCorrection.setTextColor(getResources().getColor(R.color.meal_correction_negative));
			}
			else if (shiftedCarbs > CARB_COLOR_LIMIT)
			{
				textMealCarbsCorrection.setTextColor(getResources().getColor(R.color.meal_correction_positive));
			}
			else
			{
				textMealCarbsCorrection.setTextColor(Color.BLACK);
			}

			if (insInjected < Utils.EPS && shiftedCarbs < 0)
			{
				textMealCarbsCorrection.setTypeface(Typeface.DEFAULT);
			}
			else
			{
				textMealCarbsCorrection.setTypeface(Typeface.DEFAULT_BOLD);
			}

			double carbsResult = entity.getData().getCarbs() + shiftedCarbs;
			if (carbsResult >= 0)
			{
				textMealCarbsResult.setText(String.format(Locale.US, "(%.0f %s)", carbsResult, captionGramm));
			}
			else
			{
				textMealCarbsResult.setText("");
			}
		}
		else
		{
			textMealCarbsCorrection.setText("?");
			textMealCarbsResult.setText("");
		}
	}

	private void showExpectedBs(Double inputBS, double insulinDosage, double carbs, double prots, Koof koof)
	{
		if (koof != null)
		{
			if (inputBS != null)
			{
				double expectedBS = inputBS + carbs * koof.getK() + prots * koof.getP() - insulinDosage * koof.getQ();
				if (expectedBS > BS_HYPOGLYCEMIA)
				{
					textMealExpectedBs.setText(String.format(Locale.US, "%.1f %s", expectedBS, captionMmol));
				}
				else
				{
					textMealExpectedBs.setText(getString(R.string.editor_meal_label_expected_bs_hypoglycemia));
				}
			}
			else
			{
				double deltaBS = carbs * koof.getK() + prots * koof.getP() - insulinDosage * koof.getQ();
				textMealExpectedBs.setText(String.format(Locale.US, "...%+.1f %s", deltaBS, captionMmol));
			}
		}
		else
		{
			textMealExpectedBs.setText("?");
		}
	}

	private double getInsInjected()
	{
		Bundle extras = getIntent().getExtras();
		return extras.getDouble(FIELD_INS_INJECTED, 0.0);
	}

	private double getBsTarget()
	{
		Bundle extras = getIntent().getExtras();
		return extras.getDouble(FIELD_BS_TARGET);
	}

	private Double getBsLast()
	{
		Bundle extras = getIntent().getExtras();
		return extras.containsKey(FIELD_BS_LAST) ? extras.getDouble(FIELD_BS_LAST) : null;
	}

	private Double getBsBase()
	{
		Bundle extras = getIntent().getExtras();
		return extras.containsKey(FIELD_BS_BASE) ? extras.getDouble(FIELD_BS_BASE) : null;
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		buttonTime.setText(formatTime(entity.getData().getTime()));
		buttonDate.setText(formatDate(entity.getData().getTime()));

		showMealContent();
		showMealInfo();

		if (createMode)
		{
			mealEditor.requestFocus();
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		return true;
	}

	@Override
	protected void onDateTimeChanged(Date time)
	{
		buttonTime.setText(formatTime(time));
		buttonDate.setText(formatDate(time));
		modified();
		showMealInfo();
	}

	private void modified()
	{
		entity.modified();

		new AsyncTask<Void, Void, Void>()
		{
			@Override
			protected Void doInBackground(Void... params)
			{
				if (diary == null)
				{
					diary = new DiaryLocalService(ActivityEditorMeal.this);
				}
				diary.save(Collections.singletonList(new Versioned<DiaryRecord>(entity)));
				return null;
			}
		}.execute();
	}
}