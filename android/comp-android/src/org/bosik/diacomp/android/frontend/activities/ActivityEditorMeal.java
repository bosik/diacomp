package org.bosik.diacomp.android.frontend.activities;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView.OnChangeListener;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Intent;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;

public class ActivityEditorMeal extends ActivityEditorTime<MealRecord>
{
	public static final String	TAG						= ActivityEditorMeal.class.getSimpleName();

	public static final String	FIELD_BS_BEFORE_MEAL	= "bosik.pack.bs.beforeMeal";
	public static final String	FIELD_BS_TARGET			= "bosik.pack.bs.target";
	public static final String	FIELD_INS_INJECTED		= "bosik.pack.insInjected";

	// data
	boolean						modified;
	private Double				bsBeforeMeal;
	private Double				bsTarget;
	private Double				insInjected;

	// components
	private Button				buttonTime;
	private Button				buttonDate;
	private TextView			textMealStatProts;
	private TextView			textMealStatFats;
	private TextView			textMealStatCarbs;
	private TextView			textMealStatValue;
	private TextView			textMealStatDosage;

	private LinearLayout		layoutDosageShift;
	private TextView			textMealShiftBS;
	private TextView			textMealShiftDosage;
	private TextView			textMealShiftCarbs;
	private TextView			textMealCompDosage;
	private TextView			textMealCompCarbs;
	private MealEditorView		mealEditor;

	// localization
	private String				captionProts;
	private String				captionFats;
	private String				captionCarbs;
	private String				captionValue;
	private String				captionDose;
	private String				captionGramm;
	private String				captionMmol;

	// ======================================================================================================

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_meal);

		// string constants
		captionProts = "P";
		captionFats = "F";
		captionCarbs = "C";
		captionValue = "V";
		captionDose = getString(R.string.editor_meal_label_dose);
		captionGramm = getString(R.string.common_gramm);
		captionMmol = getString(R.string.common_bs_unit_mmol);

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

		textMealStatProts = (TextView) findViewById(R.id.textMealStatProts);
		textMealStatFats = (TextView) findViewById(R.id.textMealStatFats);
		textMealStatCarbs = (TextView) findViewById(R.id.textMealStatCarbs);
		textMealStatValue = (TextView) findViewById(R.id.textMealStatValue);
		textMealStatDosage = (TextView) findViewById(R.id.textMealStatDosage);

		layoutDosageShift = (LinearLayout) findViewById(R.id.layoutMealDosageShift);
		textMealShiftBS = (TextView) findViewById(R.id.textMealShiftBS);
		textMealShiftDosage = (TextView) findViewById(R.id.textMealShiftDosage);
		textMealShiftCarbs = (TextView) findViewById(R.id.textMealShiftCarbs);
		textMealCompDosage = (TextView) findViewById(R.id.textMealCompDosage);
		textMealCompCarbs = (TextView) findViewById(R.id.textMealCompCarbs);
		mealEditor = (MealEditorView) findViewById(R.id.mealEditorMeal);

		// list.setScroll
		// list.setScrollContainer(false);

		// list.setOnTouchListener(new OnTouchListener()
		// {
		// @Override
		// public boolean onTouch(View v, MotionEvent event)
		// {
		// if (event.getAction() == MotionEvent.ACTION_MOVE)
		// {
		// return true; // Indicates that this has been handled by you and will not be
		// // forwarded further.
		// }
		// return false;
		// }
		// });

		// list.addHeaderView(findViewById(R.id.layoutMealParent));

		// setting listeners
		mealEditor.setOnChangeListener(new OnChangeListener()
		{
			@Override
			public void onChange(List<FoodMassed> items)
			{
				modified = true;
				Log.i(TAG, "Content changed");

				entity.getData().clear();
				for (FoodMassed item : mealEditor.getData())
				{
					entity.getData().add(item);
				}

				showMealInfo();
			}
		});
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

	private void showMealInfo()
	{
		// insulin dosage info

		// FIXME: doesn't updated if time changed
		int minutesTime = Utils.timeToMin(entity.getData().getTime());

		// TODO: hackfix
		Koof koof = Storage.koofService.getKoof(minutesTime);

		double deltaBS = (bsBeforeMeal != null && bsTarget != null) ? bsTarget - bsBeforeMeal : 0.0;

		double carbs = entity.getData().getCarbs();
		double prots = entity.getData().getProts();

		Double expectedBS = bsBeforeMeal == null ? null : bsBeforeMeal + (carbs * koof.getK()) + (prots * koof.getP())
				- (insInjected * koof.getQ());

		// textMealStatProts.setText(String.format("%s %.1f", captionProts,
		// entity.getData().getProts()));
		// textMealStatFats.setText(String.format("%s %.1f", captionFats,
		// entity.getData().getFats()));
		// textMealStatCarbs.setText(String.format("%s %.1f", captionCarbs,
		// entity.getData().getCarbs()));
		// textMealStatValue.setText(String.format("%s %.1f", captionValue,
		// entity.getData().getValue()));
		// textMealStatDosage.setText(String.format("%.1f %s", insInjected, captionDose));

		// shift dosage

		if (Math.abs(deltaBS) > 0.00001)
		{
			layoutDosageShift.setVisibility(View.VISIBLE);

			double shiftDose = (-deltaBS + (carbs * koof.getK()) + (prots * koof.getP())) / koof.getQ();
			double shiftCarbs = (insInjected * koof.getQ() - prots * koof.getP() + deltaBS) / koof.getK() - carbs;

			textMealShiftBS.setText(Utils.formatDoubleSigned(deltaBS) + " " + captionMmol);
			final String shiftDosage = insInjected > 0 ? String.format("%.1f / %.1f %s", shiftDose, insInjected,
					captionDose) : String.format("%.1f %s", shiftDose, captionDose);
			textMealShiftDosage.setText(shiftDosage);
			textMealShiftCarbs.setText(Utils.formatDoubleSigned(shiftCarbs) + " " + captionGramm);
			if (shiftCarbs < 0)
			{
				textMealShiftCarbs.setTextColor(getResources().getColor(R.color.meal_correction_negative));
			}
			else
			{
				textMealShiftCarbs.setTextColor(getResources().getColor(R.color.meal_correction_positive));
			}
		}
		else
		{
			layoutDosageShift.setVisibility(View.GONE);
		}

		// pure compensation dosage

		double compDose = (carbs * koof.getK() + (prots * koof.getP())) / koof.getQ();
		double compCarbs = (insInjected * koof.getQ() - prots * koof.getP()) / koof.getK() - carbs;

		final String compDosage = insInjected > 0 ? String.format("%.1f / %.1f %s", compDose, insInjected, captionDose)
				: String.format("%.1f %s", compDose, captionDose);
		textMealCompDosage.setText(compDosage);
		textMealCompCarbs.setText(Utils.formatDoubleSigned(compCarbs) + " " + captionGramm);
		if (compCarbs < 0)
		{
			textMealCompCarbs.setTextColor(getResources().getColor(R.color.meal_correction_negative));
		}
		else
		{
			textMealCompCarbs.setTextColor(getResources().getColor(R.color.meal_correction_positive));
		}

		// TODO: print expectedBS somehow

		// before = null, target = null --> deltaBS = 0.0; --> dose
		// before = null, target != null --> deltaBS = 0.0; --> dose
		// before != null, target = null --> deltaBS = 0.0; --> dose, expectedBS
		// before != null, target != null --> deltaBS = target - before; --> dose, expectedBS

		// expectedBS = f(before, meal) // undefined, if before == null
		// dose = g(deltaBS, meal) // deltaBS = target - before (if both are not-null) : 0.0 (if
		// something is not defined)
	}

	@Override
	protected void readEntity(Intent intent)
	{
		super.readEntity(intent);

		bsBeforeMeal = intent.getExtras().containsKey(FIELD_BS_BEFORE_MEAL) ? intent.getExtras().getDouble(
				FIELD_BS_BEFORE_MEAL) : null;
		bsTarget = intent.getExtras().containsKey(FIELD_BS_TARGET) ? intent.getExtras().getDouble(FIELD_BS_TARGET)
				: null;
		insInjected = intent.getExtras().getDouble(FIELD_INS_INJECTED, 0.0);
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
		}

		showMealContent();
		showMealInfo();
		modified = false;
		Log.i(TAG, "Mod flag reseted");
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
			// TODO: localization
			UIUtils.showTip(this, "Meal saved");
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
		Log.i(TAG, "Time changed");
		// TODO: change koofs & update dosage stats
	}
}