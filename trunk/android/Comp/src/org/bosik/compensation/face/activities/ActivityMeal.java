package org.bosik.compensation.face.activities;

import java.text.DecimalFormat;
import org.bosik.compensation.bo.common.FoodMassed;
import org.bosik.compensation.bo.diary.records.MealRecord;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.face.R;
import org.bosik.compensation.persistence.repository.Storage;
import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.inputmethod.EditorInfo;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;

public class ActivityMeal extends Activity
{
	// отладочная печать
	private static final String			TAG			= ActivityMeal.class.getSimpleName();

	private static final DecimalFormat	df			= new DecimalFormat("###.#");

	// переменные
	private static MealRecord			meal		= new MealRecord();
	private String						lv_arr[]	= {/*
														 * "Суп сырный (310)",
														 * "Хлеб черный \"Премиум\" (47)",
														 * "Фасоль тушеная (249)",
														 * "Молоко \"Вкуснотеево\", 3,2% (41)",
														 * "Печенье \"Юбилейное\" молочное (25)",
														 * "Сыр \"Король Артур\" (40)",
														 * "Суп сырный (310)",
														 * "Хлеб черный \"Премиум\" (47)",
														 * "Фасоль тушеная (249)",
														 * "Молоко \"Вкуснотеево\", 3,2% (41)",
														 * "Печенье \"Юбилейное\" молочное (25)",
														 * "Сыр \"Король Артур\" (40)"
														 */};

	// компоненты
	private AutoCompleteTextView		editName;
	private EditText					editMass;
	private Button						buttonAdd;
	private Activity					activityMeal;
	private ListView					list;
	private TextView					textMealCarbs;
	private TextView					textMealDose;

	private String						captionCarbs;
	private String						captionDose;

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.editor_meal);
		activityMeal = this;

		// компоненты
		editName = (AutoCompleteTextView) findViewById(R.id.autoCompleteTextView1);
		editMass = (EditText) findViewById(R.id.editItemMass);
		buttonAdd = (Button) findViewById(R.id.button_additem);
		list = (ListView) findViewById(R.id.ListView01);
		textMealCarbs = (TextView) findViewById(R.id.textMealCarbs);
		textMealDose = (TextView) findViewById(R.id.textMealDose);

		// текст
		captionCarbs = getString(R.string.label_mealCarbs);
		captionDose = getString(R.string.label_mealDose);

		// инициализация

		Log.d(TAG, "Caption carbs: " + captionCarbs);
		Log.d(TAG, "Caption dose: " + captionDose);
		Log.d(TAG, "FoodItem count: " + Storage.foodBase.count());

		loadFoodList();
		showMeal();

		editMass.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
		editMass.setOnEditorActionListener(new TextView.OnEditorActionListener()
		{
			// @Override
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
		buttonAdd.setOnClickListener(clickListener);
	}

	private void loadFoodList()
	{
		String[] foodBase = new String[Storage.foodBase.count()];
		for (int i = 0; i < Storage.foodBase.count(); i++)
		{
			// TODO: implement method returning names array sorted by tag
			foodBase[i] = Storage.foodBase.get(i).getName();
			// Log.v(TAG, foodBase[i]);
		}

		ArrayAdapter<String> baseAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, foodBase);
		editName.setAdapter(baseAdapter);
	}

	private void showMeal()
	{
		// lv_arr = mealList.toArray(new String[0]);
		lv_arr = new String[meal.count()];
		for (int i = 0; i < meal.count(); i++)
		{
			lv_arr[i] = meal.get(i).toString();
		}

		ArrayAdapter<String> adapter = new ArrayAdapter<String>(activityMeal, android.R.layout.simple_list_item_1,
				lv_arr);
		list.setAdapter(adapter);

		double dose = meal.getCarbs() * 0.155;
		textMealCarbs.setText(df.format(meal.getCarbs()) + " " + captionCarbs);
		textMealDose.setText(df.format(dose) + " " + captionDose);
	}

	private void addItem()
	{
		String name = editName.getText().toString();
		double mass = Double.parseDouble(editMass.getText().toString());

		FoodItem food = null;
		FoodMassed item = null;

		// TODO: create generics-based search
		for (int i = 0; i < Storage.foodBase.count(); i++)
		{
			if (Storage.foodBase.get(i).getName().equalsIgnoreCase(name))
			{
				food = Storage.foodBase.get(i);

				item = new FoodMassed();
				item.setName(food.getName());
				item.setRelProts(food.getRelProts());
				item.setRelFats(food.getRelFats());
				item.setRelCarbs(food.getRelCarbs());
				item.setRelValue(food.getRelValue());
				item.setMass(mass);

				meal.add(item);

				break;
			}
		}

		showMeal();

		if (item != null)
		{
			Log.e("XXX", "ADDED: " + item);
			editMass.setText("");
			editName.setText("");
			Log.d(TAG, "Moving focus to name field");
			editName.requestFocus();
		}
	}

	private OnClickListener	clickListener	= new OnClickListener()
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
											};
}
