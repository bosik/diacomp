package org.bosik.diacomp.android.frontend.activities;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.views.fdpicker.FoodDishPicker;
import org.bosik.diacomp.android.frontend.views.fdpicker.FoodDishPicker.OnSubmitListener;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.utils.Utils;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.content.DialogInterface;
import android.text.InputType;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.ListView;
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
	private ListView		list;
	private FoodDishPicker	fdPicker;

	// localization
	private String			captionCarbs;
	private String			captionDose;
	private String			captionGramm;

	// ======================================================================================================

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_meal);

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
		list = (ListView) findViewById(R.id.ListView01);
		fdPicker = (FoodDishPicker) findViewById(R.id.fdPickerMeal);

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
		list.setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View view, final int position, long id)
			{
				Builder builder = new AlertDialog.Builder(ActivityEditorMeal.this);
				final String message = entity.getData().get(position).getName() + ", " + captionGramm;

				final EditText input = new EditText(ActivityEditorMeal.this);

				input.setText(Utils.formatDoubleShort(entity.getData().get(position).getMass()));
				input.setInputType(InputType.TYPE_NUMBER_FLAG_DECIMAL);
				builder.setTitle("Change mass");
				builder.setMessage(message);
				builder.setView(input);
				builder.setPositiveButton("Ok", new DialogInterface.OnClickListener()
				{
					@Override
					public void onClick(DialogInterface dialog, int whichButton)
					{
						String text = input.getText().toString();

						try
						{
							if (text.isEmpty())
							{
								entity.getData().remove(position);
							}
							else
							{
								double mass = Utils.parseExpression(text);
								if (mass > Utils.EPS)
								{
									entity.getData().get(position).setMass(mass);
								}
								else
								{
									entity.getData().remove(position);
								}
							}
							modified = true;
							showMeal();
						}
						catch (NumberFormatException e)
						{
							// TODO: localize
							UIUtils.showTip(ActivityEditorMeal.this, "Wrong mass");
						}

					}
				});
				builder.setNegativeButton("Cancel", new DialogInterface.OnClickListener()
				{
					@Override
					public void onClick(DialogInterface dialog, int whichButton)
					{
						// Do nothing.
					}
				});
				builder.show();
			}
		});
		fdPicker.setOnSubmitLister(new OnSubmitListener()
		{
			@Override
			public boolean onSubmit(String name, double mass)
			{
				// try to search item in food base

				Versioned<FoodItem> foodItem = Storage.localFoodBase.findOne(name);

				if (foodItem != null)
				{
					FoodItem food = foodItem.getData();

					FoodMassed item = new FoodMassed();
					item.setName(food.getName());
					item.setRelProts(food.getRelProts());
					item.setRelFats(food.getRelFats());
					item.setRelCarbs(food.getRelCarbs());
					item.setRelValue(food.getRelValue());
					item.setMass(mass);

					entity.getData().add(item);
					modified = true;

					showMeal();
					return true;
				}

				// try to search item in dish base

				List<Versioned<DishItem>> listDish = Storage.localDishBase.findAny(name);

				if (!listDish.isEmpty())
				{
					DishItem dish = listDish.get(0).getData();

					FoodMassed item = new FoodMassed();
					item.setName(dish.getName());
					item.setRelProts(dish.getRelProts());
					item.setRelFats(dish.getRelFats());
					item.setRelCarbs(dish.getRelCarbs());
					item.setRelValue(dish.getRelValue());
					item.setMass(mass);

					entity.getData().add(item);

					showMeal();
					return true;
				}

				UIUtils.showTip(ActivityEditorMeal.this, "Item not found: " + name);
				fdPicker.focusName();
				return false;
			}
		});
	}

	void showMeal()
	{
		final String[] temp = new String[entity.getData().count()];
		for (int i = 0; i < entity.getData().count(); i++)
		{
			temp[i] = "stub";
		}

		ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_2,
				android.R.id.text1, temp)
		{
			@Override
			public View getView(int position, View convertView, ViewGroup parent)
			{
				View view = super.getView(position, convertView, parent);
				TextView text1 = (TextView) view.findViewById(android.R.id.text1);
				TextView text2 = (TextView) view.findViewById(android.R.id.text2);

				text1.setText(entity.getData().get(position).getName());
				text2.setText(Utils.formatDoubleShort(entity.getData().get(position).getMass()) + " " + captionGramm);
				return view;
			}
		};
		list.setAdapter(adapter);

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

		// other data is already there

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