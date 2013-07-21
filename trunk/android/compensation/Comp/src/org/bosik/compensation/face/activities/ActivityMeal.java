package org.bosik.compensation.face.activities;

import java.text.DecimalFormat;
import org.bosik.compensation.face.R;
import org.bosik.compensation.persistence.repository.Storage;
import org.bosik.compensation.persistence.entity.common.FoodMassed;
import org.bosik.compensation.persistence.entity.diary.records.MealRecord;
import org.bosik.compensation.persistence.entity.foodbase.Food;
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
	private static final String TAG = ActivityMeal.class.getSimpleName();

	private static final DecimalFormat df = new DecimalFormat("###.#");
	
	// переменные
	private static MealRecord meal = new MealRecord();
	private String lv_arr[] = {/*
								 * "Суп сырный (310)", "Хлеб черный \"Премиум\" (47)",
								 * "Фасоль тушеная (249)", "Молоко \"Вкуснотеево\", 3,2% (41)",
								 * "Печенье \"Юбилейное\" молочное (25)",
								 * "Сыр \"Король Артур\" (40)", "Суп сырный (310)",
								 * "Хлеб черный \"Премиум\" (47)", "Фасоль тушеная (249)",
								 * "Молоко \"Вкуснотеево\", 3,2% (41)",
								 * "Печенье \"Юбилейное\" молочное (25)",
								 * "Сыр \"Король Артур\" (40)"
								 */};

	// компоненты
	private AutoCompleteTextView editName;
	private EditText editMass;
	private Button buttonAdd;
	private Activity activityMeal;
	private ListView list;
	private TextView textMealCarbs;

	private String captionCarbs;
	private String captionDose;

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

		// текст
		captionCarbs = getString(R.string.label_mealCarbs);
		captionDose = getString(R.string.label_mealDose);

		// инициализация

		Log.e(TAG, "Food count: " + Storage.localFoodbase.count());

		String[] foodBase = new String[Storage.localFoodbase.count()];
		for (int i = 0; i < Storage.localFoodbase.count(); i++)
		{
			// TODO: implement method returning names array sorted by tag
			foodBase[i] = Storage.localFoodbase.get(i).getName();
			// Log.v(TAG, foodBase[i]);
		}

		ArrayAdapter<String> baseAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, foodBase);
		editName.setAdapter(baseAdapter);
		editMass.setImeOptions(EditorInfo.IME_ACTION_SEARCH);

		// lv_arr = mealList.toArray(new String[0]);
		lv_arr = new String[meal.size()];
		for (int i = 0; i < meal.size(); i++)
		{
			lv_arr[i] = meal.get(i).getName();
		}

		ArrayAdapter<String> itemsAdapter = new ArrayAdapter<String>(activityMeal, android.R.layout.simple_list_item_1,
				lv_arr);
		list.setAdapter(itemsAdapter);

		editMass.setOnEditorActionListener(new TextView.OnEditorActionListener()
		{
			// @Override
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
				return false;
			}
		});

		// назначаем обработчики
		buttonAdd.setOnClickListener(clickListener);
	}

	private OnClickListener clickListener = new OnClickListener()
	{
		public void onClick(View v)
		{
			switch (v.getId())
			{
				case R.id.button_additem:
					String name = editName.getText().toString();
					double mass = Double.parseDouble(editMass.getText().toString());

					Food food = null;
					FoodMassed item = null;

					// TODO: create generics-based search
					for (int i = 0; i < Storage.localFoodbase.count(); i++)
					{
						if (Storage.localFoodbase.get(i).getName().equalsIgnoreCase(name))
						{
							food = Storage.localFoodbase.get(i);

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

					// lv_arr = (String[]) temp.toArray(new String[temp.size()]);

					/*
					 * for (String item : temp) { Log.w("XXX", "["+item+"]"); }
					 */

					/*
					 * for (String item : lv_arr) { Log.w("XXX", "{"+item+"}"); }
					 */

					// lv_arr = mealList.toArray(new String[0]);
					lv_arr = new String[meal.size()];
					for (int i = 0; i < meal.size(); i++)
					{
						lv_arr[i] = meal.get(i).toString();
					}

					ArrayAdapter<String> adapter = new ArrayAdapter<String>(activityMeal,
							android.R.layout.simple_list_item_1, lv_arr);
					list.setAdapter(adapter);

					if (item != null)
					{
						Log.e("XXX", "ADDED: " + item);
						editMass.setText("");
						editName.setText("");
						editName.requestFocus();

						textMealCarbs.setText("Итого: " + df.format(meal.getCarbs()) + " г угл. / " + df.format(meal.getCarbs() * 0.155) + " ед.");
					}
					break;
			}
		}
	};
}
