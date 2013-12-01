package org.bosik.compensation.face.activities;

import java.text.DecimalFormat;
import java.util.List;
import org.bosik.compensation.bo.common.FoodMassed;
import org.bosik.compensation.bo.diary.records.MealRecord;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.face.R;
import org.bosik.compensation.persistence.Storage;
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
	private static final String			TAG		= ActivityMeal.class.getSimpleName();

	private static final DecimalFormat	df		= new DecimalFormat("###.#");

	// переменные
	private static MealRecord			meal	= new MealRecord();

	// компоненты
	private AutoCompleteTextView		editName;
	private EditText					editMass;
	private Button						buttonAdd;
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
		Log.d(TAG, "FoodItem count: " + Storage.foodBase.size());

		loadFoodList();
		showMeal();

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
	}

	private void loadFoodList()
	{
		String[] foodBase = new String[Storage.foodBase.size()];
		for (int i = 0; i < Storage.foodBase.size(); i++)
		{
			// TODO: implement method returning names array sorted by tag
			foodBase[i] = Storage.foodBase.get(i).getName();
			// Log.v(TAG, foodBase[i]);
		}

		ArrayAdapter<String> baseAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, foodBase);
		editName.setAdapter(baseAdapter);
	}

	public static String printFoodMassed(FoodMassed food)
	{
		return food.getName() + " (" + df.format(food.getMass()) + ")";
	}

	private void showMeal()
	{
		String[] temp = new String[meal.count()];
		for (int i = 0; i < meal.count(); i++)
		{
			temp[i] = printFoodMassed(meal.get(i));
		}

		ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, temp);
		list.setAdapter(adapter);

		double dose = meal.getCarbs() * 0.155;
		textMealCarbs.setText(df.format(meal.getCarbs()) + " " + captionCarbs);
		textMealDose.setText(df.format(dose) + " " + captionDose);
	}

	private void addItem()
	{
		String name = editName.getText().toString();
		double mass = Double.parseDouble(editMass.getText().toString());

		List<FoodItem> list = Storage.localFoodBase.findAny(name);

		if (!list.isEmpty())
		{
			FoodItem food = list.get(0);

			FoodMassed item = new FoodMassed();
			item.setName(food.getName());
			item.setRelProts(food.getRelProts());
			item.setRelFats(food.getRelFats());
			item.setRelCarbs(food.getRelCarbs());
			item.setRelValue(food.getRelValue());
			item.setMass(mass);

			meal.add(item);

			showMeal();

			Log.v("XXX", "ADDED: " + item);
			editMass.setText("");
			editName.setText("");
			Log.v(TAG, "Moving focus to name field");
			editName.requestFocus();
		}
	}
}
