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
import android.widget.DatePicker;
import android.widget.DatePicker.OnDateChangedListener;
import android.widget.TextView;
import android.widget.TimePicker;
import android.widget.TimePicker.OnTimeChangedListener;

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
	private TimePicker			timePicker;
	private DatePicker			datePicker;
	private TextView			textMealCarbs;
	private TextView			textMealCorrection;
	private TextView			textMealDose;
	private MealEditorView		mealEditor;

	// localization
	private String				captionCarbs;
	private String				captionDose;
	private String				captionGramm;

	// ======================================================================================================

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_meal);

		// string constants
		captionCarbs = getString(R.string.editor_meal_label_carbs);
		captionDose = getString(R.string.editor_meal_label_dose);
		captionGramm = getString(R.string.common_gramm);

		// components
		timePicker = (TimePicker) findViewById(R.id.pickerMealTime);
		timePicker.setIs24HourView(true);
		datePicker = (DatePicker) findViewById(R.id.pickerMealDate);
		textMealCarbs = (TextView) findViewById(R.id.textMealCarbs);
		textMealCorrection = (TextView) findViewById(R.id.textMealCorrection);
		textMealDose = (TextView) findViewById(R.id.textMealDose);
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
		timePicker.setOnTimeChangedListener(new OnTimeChangedListener()
		{
			@Override
			public void onTimeChanged(TimePicker view, int hourOfDay, int minute)
			{
				modified = true;
				Log.i(TAG, "Time changed");
			}
		});
		datePicker.init(2000, 06, 06, new OnDateChangedListener()
		{
			// the initial values doesn't matter
			@Override
			public void onDateChanged(DatePicker view, int year, int monthOfYear, int dayOfMonth)
			{
				modified = true;
				Log.i(TAG, "Date changed");
			}
		});

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
		double dose = (-deltaBS + (carbs * koof.getK()) + (prots * koof.getP())) / koof.getQ();
		Double expectedBS = bsBeforeMeal == null ? null : bsBeforeMeal + (carbs * koof.getK()) + (prots * koof.getP())
				- (insInjected * koof.getQ());
		double correctionCarbs = (insInjected * koof.getQ() - prots * koof.getP() + deltaBS) / koof.getK() - carbs;

		textMealCarbs.setText(String.format("%.1f %s", carbs, captionCarbs));
		textMealCorrection.setText(Utils.formatDoubleSigned(correctionCarbs) + " " + captionGramm);
		textMealDose.setText(String.format("%.1f %s", dose, captionDose));
		// TODO: print expectedBS somehow
		
		if (correctionCarbs < 0)
		{
			textMealCorrection.setTextColor(getResources().getColor(R.color.meal_correction_negative));
		}
		else
		{
			textMealCorrection.setTextColor(getResources().getColor(R.color.meal_correction_positive));
		}
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
			showTime(entity.getData().getTime(), datePicker, timePicker);
		}
		else
		{
			showTime(new Date(), datePicker, timePicker);
		}

		showMealContent();
		showMealInfo();
		modified = false;
		Log.i(TAG, "Mod flag reseted");
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		// time
		try
		{
			entity.getData().setTime(readTime(datePicker, timePicker));
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, ERROR_INCORRECT_TIME);
			timePicker.requestFocus();
			return false;
		}

		// content is already there
		// ...

		// done
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
}