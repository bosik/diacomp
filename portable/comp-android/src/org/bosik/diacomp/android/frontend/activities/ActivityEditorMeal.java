package org.bosik.diacomp.android.frontend.activities;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView.OnChangeListener;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.utils.Utils;
import android.content.Intent;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
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
	Double						insInjected;

	static boolean				correctBs				= true;

	// components
	private Button				buttonTime;
	private Button				buttonDate;
	// private TextView textMealStatProts;
	// private TextView textMealStatFats;
	// private TextView textMealStatCarbs;
	// private TextView textMealStatValue;
	// private TextView textMealStatDosage;

	private LinearLayout		layoutShifted;
	private TextView			textMealCurrentDosage;
	private TextView			textMealShiftedCarbs;
	private TextView			textMealShiftedDosage;
	private TextView			textMealExpectedBs;
	Button						buttonCorrection;
	private Button				buttonStatistics;
	MealEditorView				mealEditor;

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

		// textMealStatProts = (TextView) findViewById(R.id.textMealStatProts);
		// textMealStatFats = (TextView) findViewById(R.id.textMealStatFats);
		// textMealStatCarbs = (TextView) findViewById(R.id.textMealStatCarbs);
		// textMealStatValue = (TextView) findViewById(R.id.textMealStatValue);
		// textMealStatDosage = (TextView) findViewById(R.id.textMealStatDosage);

		layoutShifted = (LinearLayout) findViewById(R.id.layoutMealShifted);
		textMealCurrentDosage = (TextView) findViewById(R.id.textMealCurrentDosage);
		textMealShiftedCarbs = (TextView) findViewById(R.id.textMealShiftedCarbs);
		textMealShiftedDosage = (TextView) findViewById(R.id.textMealShiftedDosage);
		textMealExpectedBs = (TextView) findViewById(R.id.textMealExpectedBs);
		buttonCorrection = (Button) findViewById(R.id.buttonMealCorrection);
		buttonStatistics = (Button) findViewById(R.id.buttonMealInfo);

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

		buttonCorrection.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				if ((insInjected != null) && (insInjected > Utils.EPS))
				{
					ActivityEditorMeal.correctBs = !ActivityEditorMeal.correctBs;
					ActivityEditorMeal.this.showMealInfo();

					if (ActivityEditorMeal.correctBs)
					{
						buttonCorrection.setText("\\");
					}
					else
					{
						buttonCorrection.setText("—");
					}
				}
			}
		});

		buttonStatistics.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				String info = String.format("Prots: %.1f\nFats: %.1f\nCarbs: %.1f\nValue: %.1f\nMass: %.0f", entity
						.getData().getProts(), entity.getData().getFats(), entity.getData().getCarbs(), entity
						.getData().getValue(), entity.getData().getMass());
				UIUtils.showTip(ActivityEditorMeal.this, info);
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
		switch (item.getItemId())
		{
			case R.id.item_meal_short:
			{
				boolean newValue = !entity.getData().getShortMeal();
				entity.getData().setShortMeal(newValue);
				modified = true;
				item.setChecked(newValue);
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
		// insulin dosage info

		// FIXME: doesn't updated if time changed
		// FIXME: hardcode
		int minutesTime = (Utils.timeToMin(entity.getData().getTime()) + 4 * 60) % Utils.MinPerDay;

		// TODO: hackfix
		Koof koof = Storage.koofService.getKoof(minutesTime);

		Log.i(TAG, "Time: " + minutesTime);
		Log.i(TAG, "k = " + koof.getK());
		Log.i(TAG, "q = " + koof.getQ());
		Log.i(TAG, "p = " + koof.getP());

		double deltaBS = 0.0;

		if (bsBeforeMeal != null && bsTarget != null)
		{
			if (correctBs)
			{
				deltaBS = bsTarget - bsBeforeMeal;
			}
		}

		double carbs = entity.getData().getCarbs();
		double prots = entity.getData().getProts();

		Double expectedBS = bsBeforeMeal == null ? null : bsBeforeMeal + (carbs * koof.getK()) + (prots * koof.getP())
				- (insInjected * koof.getQ());

		if (expectedBS != null)
		{
			textMealExpectedBs.setText(String.format("%.1f %s", expectedBS, captionMmol));
		}
		else
		{
			textMealExpectedBs.setText("?");
		}

		// textMealStatProts.setText(String.format("%s %.1f", captionProts,
		// entity.getData().getProts()));
		// textMealStatFats.setText(String.format("%s %.1f", captionFats,
		// entity.getData().getFats()));
		// textMealStatCarbs.setText(String.format("%s %.1f", captionCarbs,
		// entity.getData().getCarbs()));
		// textMealStatValue.setText(String.format("%s %.1f", captionValue,
		// entity.getData().getValue()));
		// textMealStatDosage.setText(String.format("%.1f %s", insInjected, captionDose));

		// current dosage

		double currentDose = (-deltaBS + carbs * koof.getK() + (prots * koof.getP())) / koof.getQ();
		textMealCurrentDosage.setText(String.format("%.1f %s", currentDose, captionDose));

		// shifted dosage

		if (insInjected > Utils.EPS)
		{
			layoutShifted.setVisibility(View.VISIBLE);

			double shiftedCarbs = (insInjected * koof.getQ() - prots * koof.getP() + deltaBS) / koof.getK() - carbs;
			if (shiftedCarbs < 0)
			{
				textMealShiftedCarbs.setTextColor(getResources().getColor(R.color.meal_correction_negative));
			}
			else
			{
				textMealShiftedCarbs.setTextColor(getResources().getColor(R.color.meal_correction_positive));
			}
			textMealShiftedCarbs.setText(Utils.formatDoubleSigned(shiftedCarbs) + " " + captionGramm);

			textMealShiftedDosage.setText(String.format("%.1f %s", insInjected, captionDose));
		}
		else
		{
			layoutShifted.setVisibility(View.GONE);
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