package org.bosik.diacomp.android.frontend.activities;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.content.DialogInterface;
import android.os.Bundle;
import android.text.InputType;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;

public class ActivityMeal extends ActivityEditor<MealRecord>
{
	// отладочная печать
	private static final String			TAG	= ActivityMeal.class.getSimpleName();

	private static final DecimalFormat	df	= new DecimalFormat("###.#");

	// data
	private List<Versioned<FoodItem>>	foodBase;
	private List<Versioned<DishItem>>	dishBase;

	// компоненты
	private AutoCompleteTextView		editName;
	private EditText					editMass;
	private Button						buttonAdd;
	private ListView					list;
	private TextView					textMealCarbs;
	private TextView					textMealDose;
	private Button						buttonOK;

	private String						captionCarbs;
	private String						captionDose;

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_meal);

		// компоненты
		editName = (AutoCompleteTextView) findViewById(R.id.autoCompleteTextView1);
		editMass = (EditText) findViewById(R.id.editItemMass);
		buttonAdd = (Button) findViewById(R.id.button_additem);
		list = (ListView) findViewById(R.id.ListView01);
		list.setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View view, final int position, long id)
			{
				Builder builder = new AlertDialog.Builder(ActivityMeal.this);
				final String message = entity.getData().get(position).getName();

				final EditText input = new EditText(ActivityMeal.this);

				// Utils.
				input.setText(String.valueOf(entity.getData().get(position).getMass()));
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
							double mass = Utils.parseDouble(text);
							if (mass > Utils.EPS)
							{
								entity.getData().get(position).setMass(mass);
							}
							else
							{
								entity.getData().remove(position);
							}
							showMeal();
						}
						catch (ParseException e)
						{
							// TODO: localize
							UIUtils.showTip(ActivityMeal.this, "Wrong mass");
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

		textMealCarbs = (TextView) findViewById(R.id.textMealCarbs);
		textMealDose = (TextView) findViewById(R.id.textMealDose);

		// текст
		captionCarbs = getString(R.string.editor_meal_label_carbs);
		captionDose = getString(R.string.editor_meal_label_dose);

		// инициализация

		Log.d(TAG, "Caption carbs: " + captionCarbs);
		Log.d(TAG, "Caption dose: " + captionDose);

		loadItemsList();

		editMass.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
		editMass.setOnEditorActionListener(new TextView.OnEditorActionListener()
		{
			@Override
			public boolean onEditorAction(TextView v, int actionId, KeyEvent event)
			{
				// super onEditorAction(v, actionId, event);
				Log.d("XXX", "actionId = " + actionId);
				Log.d("XXX", "EditorInfo.IME_ACTION_SEARCH = " + EditorInfo.IME_ACTION_SEARCH);

				if (actionId == EditorInfo.IME_ACTION_SEARCH)
				{
					// editMass.requestFocus();
					Log.d("XXX", "It works!");
					return true;
				}

				if (actionId == EditorInfo.IME_ACTION_UNSPECIFIED)
				{
					Log.d("XXX", "Enter pressed");
					addItem();
					// editName.requestFocus();
					return true;
				}

				return false;
			}
		});

		// назначаем обработчики
		buttonAdd.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				switch (v.getId())
				{
					case R.id.button_additem:
						addItem();
						break;
				}
			}
		});

		buttonOK = (Button) findViewById(R.id.buttonMealOK);
		buttonOK.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityMeal.this.submit();
			}
		});
	}

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
	}

	private void loadItemsList()
	{
		foodBase = Storage.localFoodBase.findAll(false);
		dishBase = Storage.localDishBase.findAll(false);
		String[] items = new String[foodBase.size() + dishBase.size()];

		int i = 0;
		for (Versioned<FoodItem> food : foodBase)
		{
			items[i++] = food.getData().getName();
		}
		for (Versioned<DishItem> dish : dishBase)
		{
			items[i++] = dish.getData().getName();
		}

		ArrayAdapter<String> baseAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, items);
		editName.setAdapter(baseAdapter);
	}

	public static String printFoodMassed(FoodMassed food)
	{
		return food.getName() + " (" + df.format(food.getMass()) + ")";
	}

	private void showMeal()
	{
		String[] temp = new String[entity.getData().count()];
		for (int i = 0; i < entity.getData().count(); i++)
		{
			temp[i] = printFoodMassed(entity.getData().get(i));
		}

		ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, temp);
		list.setAdapter(adapter);

		// insulin dosage info
		double dose = entity.getData().getCarbs() * 0.155;
		textMealCarbs.setText(df.format(entity.getData().getCarbs()) + " " + captionCarbs);
		textMealDose.setText(df.format(dose) + " " + captionDose);
	}

	void addItem()
	{
		String name = editName.getText().toString();
		if (name.trim().isEmpty())
		{
			// TODO: localize
			UIUtils.showTip(this, "Enter food or dish name");
			editName.requestFocus();
			return;
		}

		double mass;
		try
		{
			mass = Double.parseDouble(editMass.getText().toString());
		}
		catch (NumberFormatException e)
		{
			// TODO: localize
			UIUtils.showTip(this, "Enter food or dish name");
			editMass.requestFocus();
			return;
		}

		// try to search item in food base

		List<Versioned<FoodItem>> listFood = Storage.localFoodBase.findAny(name);

		if (!listFood.isEmpty())
		{
			FoodItem food = listFood.get(0).getData();

			FoodMassed item = new FoodMassed();
			item.setName(food.getName());
			item.setRelProts(food.getRelProts());
			item.setRelFats(food.getRelFats());
			item.setRelCarbs(food.getRelCarbs());
			item.setRelValue(food.getRelValue());
			item.setMass(mass);

			entity.getData().add(item);

			showMeal();

			Log.v("XXX", "ADDED: " + item);
			editMass.setText("");
			editName.setText("");
			Log.v(TAG, "Moving focus to name field");
			editName.requestFocus();

			return;
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

			Log.v("XXX", "ADDED: " + item);
			editMass.setText("");
			editName.setText("");
			Log.v(TAG, "Moving focus to name field");
			editName.requestFocus();

			return;
		}

		UIUtils.showTip(this, "Item not found: " + name);
		editMass.requestFocus();
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		showMeal();
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		return true;
	}
}