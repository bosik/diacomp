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
import android.widget.DatePicker;
import android.widget.TextView;
import android.widget.TimePicker;
import android.widget.TimePicker.OnTimeChangedListener;

public class ActivityEditorMeal extends ActivityEditor<MealRecord>
{
	// data
	boolean					modified;

	// components
	private TimePicker		timePicker;
	private DatePicker		datePicker;
	private TextView		textMealCarbs;
	private TextView		textMealDose;
	private MealEditorView	mealEditor;

	// localization
	private String			captionCarbs;
	private String			captionDose;
	private String			captionGramm;

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
			}
		});

		mealEditor.setOnChangeListener(new OnChangeListener()
		{
			@Override
			public void onChange(List<FoodMassed> items)
			{
				modified = true;
			}
		});
	}

	void showMeal()
	{
		List<FoodMassed> items = new ArrayList<FoodMassed>();

		for (int i = 0; i < entity.getData().count(); i++)
		{
			items.add(entity.getData().get(i));
		}

		mealEditor.setData(items);

		// insulin dosage info

		int minutesTime = Utils.timeToMin(entity.getData().getTime());

		// TODO: hackfix
		Koof koof = Storage.koofService.getKoof(minutesTime);

		double carbs = entity.getData().getCarbs();
		double prots = entity.getData().getProts();
		double dose = ((carbs * koof.getK()) + (prots * koof.getP())) / koof.getQ();
		textMealCarbs.setText(String.format("%.1f %s", carbs, captionCarbs));
		textMealDose.setText(String.format("%.1f %s", dose, captionDose));
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

		showMeal();
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

		// content

		entity.getData().clear();
		for (FoodMassed item : mealEditor.getData())
		{
			entity.getData().add(item);
		}

		// done

		modified = false;
		return true;
	}

	@Override
	public void onBackPressed()
	{
		if (modified)
		{
			submit();
		}
		else
		{
			super.onBackPressed();
		}
	}
}