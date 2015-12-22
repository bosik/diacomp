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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.analyze.KoofServiceInternal;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView.OnChangeListener;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.analyze.KoofService;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;

public class ActivityEditorMeal extends ActivityEditorTime<MealRecord>
{
	public static final String	TAG					= ActivityEditorMeal.class.getSimpleName();

	public static final String	FIELD_BS_BASE		= "bosik.pack.bs.base";
	public static final String	FIELD_BS_LAST		= "bosik.pack.bs.last";
	public static final String	FIELD_BS_TARGET		= "bosik.pack.bs.target";
	public static final String	FIELD_INS_INJECTED	= "bosik.pack.insInjected";

	// FIXME: hardcoded BS value
	private static final double	BS_HYPOGLYCEMIA		= 4.0;
	private static final double	CARB_COLOR_LIMIT	= 1.0;

	// data
	boolean						modified;
	private Double				bsBase;
	private Double				bsLast;
	private Double				bsTarget;
	private double				insInjected;

	// components
	private Button				buttonTime;
	private Button				buttonDate;

	private TextView			textMealCurrentDosage;
	private TextView			textMealShiftedCarbs;
	private TextView			textMealShiftedDosage;
	private TextView			textMealExpectedBs;
	MealEditorView				mealEditor;

	// localization
	private String				captionDose;
	private String				captionGramm;
	private String				captionMmol;

	// ======================================================================================================

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_meal);

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

		textMealCurrentDosage = (TextView) findViewById(R.id.textMealCurrentDosage);
		textMealShiftedCarbs = (TextView) findViewById(R.id.textMealShiftedCarbs);
		textMealShiftedDosage = (TextView) findViewById(R.id.textMealShiftedDosage);
		textMealExpectedBs = (TextView) findViewById(R.id.textMealExpectedBs);

		mealEditor = (MealEditorView) findViewById(R.id.mealEditorMeal);

		// list.addHeaderView(findViewById(R.id.layoutMealParent));

		// setting listeners
		mealEditor.setOnChangeListener(new OnChangeListener()
		{
			@Override
			public void onChange(List<FoodMassed> items)
			{
				modified = true;
				entity.getData().clear();
				for (FoodMassed item : mealEditor.getData())
				{
					entity.getData().add(item);
				}

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
				modified = true;
				return true;
			}
			case R.id.item_meal_info:
			{
				String s = getString(R.string.editor_meal_tip_meal_info);
				String info = String.format(s, data.getProts(), data.getFats(), data.getCarbs(), data.getValue(),
						data.getMass());
				UIUtils.showLongTip(ActivityEditorMeal.this, info);
				return true;
			}
			default:
			{
				return false;
			}
		}
	}

	void showMealContent()
	{
		List<FoodMassed> items = new ArrayList<FoodMassed>();

		for (int i = 0; i < entity.getData().count(); i++)
		{
			items.add(entity.getData().get(i));
		}

		mealEditor.setData(items);
	}

	void showMealInfo()
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
		KoofService koofService = KoofServiceInternal.getInstance(getContentResolver());
		int minutesTime = Utils.getDayMinutesUTC(entity.getData().getTime());
		Koof koof = koofService.getKoof(minutesTime);
		double carbs = entity.getData().getCarbs();
		double prots = entity.getData().getProts();

		if (bsLast != null && bsLast < BS_HYPOGLYCEMIA && (bsBase == null || Math.abs(bsBase - bsLast) > Utils.EPS))
		{
			// Line 1: Insulin
			hideInsulinDosage();

			// Line 2: Correction
			double shiftedCarbs = (prots * koof.getP() + (bsTarget - bsLast)) / koof.getK() - carbs;
			showCorrection(shiftedCarbs, null);

			// Line 3: Expected BS
			showExpectedBs(bsLast, 0, carbs, prots, koof);
		}
		else
		{
			// Line 1: Insulin
			showInsulinDosage(bsBase, bsTarget, (insInjected > 0), carbs, prots, koof);

			// Line 2: Correction
			if (insInjected > 0)
			{
				double deltaBS = (bsBase == null ? 0.0 : bsTarget - bsBase);
				double shiftedCarbs = (insInjected * koof.getQ() - prots * koof.getP() + deltaBS) / koof.getK() - carbs;
				showCorrection(shiftedCarbs, insInjected);
			}
			else
			{
				showCorrection(null, null);
			}

			// Line 3: Expected BS
			showExpectedBs(bsBase, insInjected, carbs, prots, koof);
		}
	}

	private void hideInsulinDosage()
	{
		findViewById(R.id.mealRowInsulin).setVisibility(View.GONE);
	}

	private void showInsulinDosage(Double inputBS, Double targetBS, boolean insulinInjected, double carbs, double prots,
			Koof koof)
	{
		findViewById(R.id.mealRowInsulin).setVisibility(View.VISIBLE);

		if (targetBS != null)
		{
			double deltaBS = (inputBS == null ? 0.0 : targetBS - inputBS);
			double currentDosage = (-deltaBS + carbs * koof.getK() + (prots * koof.getP())) / koof.getQ();
			if (currentDosage > 0)
			{
				textMealCurrentDosage.setText(String.format("%.1f %s", currentDosage, captionDose));
				if (insulinInjected)
				{
					textMealCurrentDosage.setTypeface(Typeface.DEFAULT);
				}
				else
				{
					textMealCurrentDosage.setTypeface(Typeface.DEFAULT_BOLD);
				}
			}
			else
			{
				textMealCurrentDosage.setTypeface(Typeface.DEFAULT);
				textMealCurrentDosage.setText("--");
			}
		}
		else
		{
			textMealCurrentDosage.setTypeface(Typeface.DEFAULT);
			textMealCurrentDosage.setText("?");
		}
	}

	void showCorrection(Double shiftedCarbs, Double insDosage)
	{
		if (shiftedCarbs != null)
		{
			findViewById(R.id.mealRowCarbs).setVisibility(View.VISIBLE);

			textMealShiftedCarbs.setText(Utils.formatDoubleSigned(shiftedCarbs) + " " + captionGramm);
			textMealShiftedCarbs.setTypeface(Typeface.DEFAULT_BOLD);
			if (shiftedCarbs < -CARB_COLOR_LIMIT)
			{
				textMealShiftedCarbs.setTextColor(getResources().getColor(R.color.meal_correction_negative));
			}
			else if (shiftedCarbs > CARB_COLOR_LIMIT)
			{
				textMealShiftedCarbs.setTextColor(getResources().getColor(R.color.meal_correction_positive));
			}
			else
			{
				textMealShiftedCarbs.setTextColor(Color.BLACK);
			}

			if (insDosage != null)
			{
				textMealShiftedDosage.setVisibility(View.VISIBLE);
				textMealShiftedDosage.setText(String.format(" → %.1f %s", insInjected, captionDose));
			}
			else
			{
				textMealShiftedDosage.setVisibility(View.GONE);
			}
		}
		else
		{
			findViewById(R.id.mealRowCarbs).setVisibility(View.GONE);
		}
	}

	private void showExpectedBs(Double inputBS, double insulinDosage, double carbs, double prots, Koof koof)
	{
		if (inputBS != null)
		{
			double expectedBS = inputBS + (carbs * koof.getK()) + (prots * koof.getP()) - (insulinDosage * koof.getQ());
			if (expectedBS > BS_HYPOGLYCEMIA)
			{
				textMealExpectedBs.setText(String.format("%.1f %s", expectedBS, captionMmol));
			}
			else
			{
				textMealExpectedBs.setText(getString(R.string.editor_meal_label_expected_bs_hypoglycemia));
			}
		}
		else
		{
			textMealExpectedBs.setText("?");
		}
	}

	@Override
	protected void readEntity(Intent intent)
	{
		super.readEntity(intent);

		final Bundle extras = intent.getExtras();

		bsBase = extras.containsKey(FIELD_BS_BASE) ? extras.getDouble(FIELD_BS_BASE) : null;
		bsLast = extras.containsKey(FIELD_BS_LAST) ? extras.getDouble(FIELD_BS_LAST) : null;
		bsTarget = extras.containsKey(FIELD_BS_TARGET) ? extras.getDouble(FIELD_BS_TARGET) : null;
		insInjected = extras.getDouble(FIELD_INS_INJECTED, 0.0);
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		if (!createMode)
		{
			onDateTimeChanged(entity.getData().getTime());
		}
		else
		{
			onDateTimeChanged(new Date());
			mealEditor.requestFocus();
		}

		showMealContent();
		showMealInfo();
		modified = false;
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		return true;
	}

	@Override
	public void onBackPressed()
	{
		if (modified)
		{
			submit();
			UIUtils.showTip(this, getString(R.string.editor_meal_message_saved));
		}
		else
		{
			super.onBackPressed();
		}
	}

	@Override
	protected void onDateTimeChanged(Date time)
	{
		buttonTime.setText(formatTime(time));
		buttonDate.setText(formatDate(time));
		modified = true;
		showMealInfo();
	}
}