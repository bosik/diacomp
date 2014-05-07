package org.bosik.diacomp.android.frontend.activities;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.search.Sorter;
import org.bosik.diacomp.core.utils.Utils;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.content.DialogInterface;
import android.text.InputType;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.SimpleAdapter;
import android.widget.TextView;
import android.widget.TimePicker;
import android.widget.TimePicker.OnTimeChangedListener;

public class ActivityEditorMeal extends ActivityEditor<MealRecord>
{
	// private static final String TAG = ActivityEditorMeal.class.getSimpleName();

	private static final DecimalFormat		df			= new DecimalFormat("###.#");
	private static final Sorter<FoodItem>	sorterFood	= new Sorter<FoodItem>();
	// private static final Sorter<DishItem> sorterDish = new Sorter<DishItem>();
	private final Map<String, Integer>		tagInfo		= Storage.tagService.getTags();

	// data
	boolean									modified	= false;

	// components
	private TimePicker						timePicker;
	private DatePicker						datePicker;
	private AutoCompleteTextView			editName;
	private EditText						editMass;
	private Button							buttonAdd;
	private ListView						list;
	private TextView						textMealCarbs;
	private TextView						textMealDose;

	private String							captionCarbs;
	private String							captionDose;
	private String							captionGramm;

	// ======================================================================================================
	// Array of strings storing country names
	// String[] countries = new String[] { "Food", "Dish" };

	// Array of integers points to images stored in /res/drawable-ldpi/
	int[]									flags		= new int[] { R.drawable.button_foodbase,
			R.drawable.button_dishbase					};

	// Array of strings to store currencies
	String[]								currency	= new String[] { "Indian Rupee", "Pakistani Rupee" };

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
		editName = (AutoCompleteTextView) findViewById(R.id.mealFoodDishInput);
		editMass = (EditText) findViewById(R.id.editItemMass);
		buttonAdd = (Button) findViewById(R.id.button_additem);
		list = (ListView) findViewById(R.id.ListView01);
		textMealCarbs = (TextView) findViewById(R.id.textMealCarbs);
		textMealDose = (TextView) findViewById(R.id.textMealDose);

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

		// filling data
		loadItemsList();
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

	private void loadItemsList()
	{
		// preparing storages
		List<Versioned<FoodItem>> foodBase = Storage.localFoodBase.findAll(false);
		List<Versioned<DishItem>> dishBase = Storage.localDishBase.findAll(false);
		// THINK: what is proper array type?
		List<Versioned<FoodItem>> fdBase = new ArrayList<Versioned<FoodItem>>();

		// filling: food
		for (Versioned<FoodItem> food : foodBase)
		{
			FoodItem data = new FoodItem(food.getData());
			Integer tag = tagInfo.get(food.getId());
			data.setTag(tag != null ? tag : 0);

			fdBase.add(new Versioned<FoodItem>(data));
		}

		// filling: dish
		for (Versioned<DishItem> dish : dishBase)
		{
			FoodItem data = new FoodItem();
			data.setName(dish.getData().getName());
			data.setRelProts(dish.getData().getRelProts());
			data.setRelFats(dish.getData().getRelFats());
			data.setRelCarbs(dish.getData().getRelCarbs());
			data.setRelValue(dish.getData().getRelValue());

			Versioned<FoodItem> item = new Versioned<FoodItem>();
			item.setId(dish.getId());
			item.setTimeStamp(dish.getTimeStamp()); // actually need?
			item.setVersion(item.getVersion()); // actually need?
			item.setDeleted(dish.isDeleted()); // actually need?
			item.setData(data);

			fdBase.add(item);
		}

		// ordering
		sorterFood.sort(fdBase, Sorter.Sort.RELEVANT);
		//
		// // showing
		// String[] items = new String[fdBase.size()];
		// int i = 0;
		// for (Versioned<FoodItem> item : fdBase)
		// {
		// items[i++] = item.getData().getName();
		// }
		//
		// ArrayAdapter<String> baseAdapter = new ArrayAdapter<String>(this,
		// android.R.layout.simple_list_item_1, items)
		// {
		// // @Override
		// // public View getView(int position, View convertView, ViewGroup parent)
		// // {
		// // View view = super.getView(position, convertView, parent);
		// // TextView text1 = (TextView) view.findViewById(android.R.id.text1);
		// // // TextView text2 = (TextView) view.findViewById(android.R.id.text2);
		// //
		// // FoodItem data = fdBase.get(position).getData();
		// // text1.setText(data.getName());
		// // // text2.setText(String.valueOf(data.getRelCarbs()));
		// // return view;
		// // }
		// };
		// editName.setAdapter(baseAdapter);

		// ================================================================================================

		List<HashMap<String, String>> aList = new ArrayList<HashMap<String, String>>();

		for (int i = 0; i < fdBase.size(); i++)
		{
			HashMap<String, String> hm = new HashMap<String, String>();
			hm.put("txt", fdBase.get(i).getData().getName());
			hm.put("flag", Integer.toString(R.drawable.button_foodbase));
			aList.add(hm);
		}

		// Keys used in Hashmap
		String[] from = { "flag", "txt" };

		// Ids of views in listview_layout
		int[] to = { R.id.itemIcon, R.id.itemDescription };

		editName.setAdapter(new SimpleAdapter(getBaseContext(), aList, R.layout.fooddishautocomplete, from, to));
		editName.setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> arg0, View arg1, int position, long id)
			{
				// HashMap<String, String> hm = (HashMap<String, String>)
				// arg0.getAdapter().getItem(position);
				// hm.get("txt"));
				editMass.requestFocus();
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
		textMealCarbs.setText(df.format(carbs) + " " + captionCarbs);
		textMealDose.setText(String.format("%.1f %s", dose, captionDose));
	}

	void addItem()
	{
		String name = editName.getText().toString();
		if (name.trim().isEmpty())
		{
			// TODO: localize
			UIUtils.showTip(this, "Enter food/dish name");
			editName.requestFocus();
			return;
		}

		double mass;
		try
		{
			mass = Utils.parseExpression(editMass.getText().toString());
		}
		catch (NumberFormatException e)
		{
			// TODO: localize
			UIUtils.showTip(this, "Enter mass");
			editMass.requestFocus();
			return;
		}

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

			editMass.setText("");
			editName.setText("");
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

			editMass.setText("");
			editName.setText("");
			editName.requestFocus();

			return;
		}

		UIUtils.showTip(this, "Item not found: " + name);
		editMass.requestFocus();
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

		return true;
	}
}