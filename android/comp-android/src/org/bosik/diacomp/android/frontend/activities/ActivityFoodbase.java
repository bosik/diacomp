package org.bosik.diacomp.android.frontend.activities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.backend.features.search.Sorter;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import android.app.Activity;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;

public class ActivityFoodbase extends Activity
{
	private static final String	TAG					= ActivityFoodbase.class.getSimpleName();

	public static final String	KEY_GUID			= "diacomp.activityfoodbase.guid";
	public static final String	KEY_MODE			= "diacomp.activityfoodbase.mode";
	public static final String	VALUE_MODE_PICK		= "diacomp.activityfoodbase.mode.pick";
	public static final String	VALUE_MODE_EDIT		= "diacomp.activityfoodbase.mode.edit";

	private static final int	DIALOG_FOOD_CREATE	= 11;
	private static final int	DIALOG_FOOD_MODIFY	= 12;

	private enum Mode
	{
		EDIT, PICK
	}

	// Widgets
	private EditText						editFoodSearch;
	private ListView						listFood;
	private Button							buttonFoodCreate;

	// Data
	final FoodBaseService					foodBaseService	= Storage.localFoodBase;
	List<Versioned<NamedRelativeTagged>>	data;
	private static final Sorter<FoodItem>	sorter			= new Sorter<FoodItem>();
	Mode									mode;
	String									searchFilter	= "";

	private long							lastSearchTime;
	private boolean							searchScheduled	= false;
	private static final long				SEARCH_DELAY	= 1000;

	// ===========================================================================

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.picker_foodbase);

		// reading intent
		Intent intent = getIntent();
		mode = VALUE_MODE_PICK.equals(intent.getStringExtra(KEY_MODE)) ? Mode.PICK : Mode.EDIT;

		// Widgets binding
		editFoodSearch = (EditText) findViewById(R.id.editFoodSearch);
		editFoodSearch.addTextChangedListener(new TextWatcher()
		{
			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count)
			{
			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after)
			{
			}

			@Override
			public void afterTextChanged(Editable s)
			{
				searchFilter = s.toString();
				runSearch(searchFilter);
			}
		});
		listFood = (ListView) findViewById(R.id.listFood);
		listFood.setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id)
			{
				final String guid = data.get(position).getId();

				switch (mode)
				{
					case PICK:
					{
						returnResult(guid);
						break;
					}
					case EDIT:
					{
						new AsyncTask<String, Void, Versioned<FoodItem>>()
						{
							@Override
							protected void onPreExecute()
							{
								// setTitle(getString(R.string.foodbase_title_loading));
							}

							@Override
							protected Versioned<FoodItem> doInBackground(String... params)
							{
								Versioned<FoodItem> food = foodBaseService.findById(guid);
								return food;
							}

							@Override
							protected void onPostExecute(Versioned<FoodItem> food)
							{
								if (food != null)
								{
									showFoodEditor(food, false);
								}
								else
								{
									UIUtils.showTip(ActivityFoodbase.this, String.format("Item %s not found", guid));
								}
							}
						}.execute(guid);

						// TODO: do the same for dish base when ready
					}
				}

			}
		});

		buttonFoodCreate = (Button) findViewById(R.id.buttonFoodCreate);
		buttonFoodCreate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View arg0)
			{
				showFoodEditor(new Versioned<FoodItem>(new FoodItem()), true);
			}
		});

		// Show data
		runSearch("");
	}

	/**
	 * Runs search process in the background thread, fills result list when done
	 * 
	 * @param key
	 */
	void runSearch(final String key)
	{
		final AsyncTask<String, Void, List<Versioned<NamedRelativeTagged>>> asyncTask = new AsyncTask<String, Void, List<Versioned<NamedRelativeTagged>>>()
		{
			@Override
			protected void onPreExecute()
			{
				setTitle(getString(R.string.foodbase_title_loading));
			}

			@Override
			protected List<Versioned<NamedRelativeTagged>> doInBackground(String... params)
			{
				return request(params[0]);
			}

			@Override
			protected void onPostExecute(List<Versioned<NamedRelativeTagged>> result)
			{
				showBase(result);
			}
		};

		TimerTask task = new TimerTask()
		{
			@Override
			public void run()
			{
				lastSearchTime = System.currentTimeMillis();
				asyncTask.execute(key);
				searchScheduled = false;
			}
		};

		if (System.currentTimeMillis() - lastSearchTime >= SEARCH_DELAY)
		{
			task.run();
		}
		else
		{
			if (!searchScheduled)
			{
				searchScheduled = true;
				new Timer().schedule(task, 1000);
			}
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.diary_menu, menu);
		return true;
	}

	/**
	 * Searches for the specified filter and returns result list
	 * 
	 * @param filter
	 * @return
	 */
	List<Versioned<NamedRelativeTagged>> request(String filter)
	{
		try
		{
			long tick = System.currentTimeMillis();

			List<Versioned<FoodItem>> temp;
			if (filter.trim().isEmpty())
			{
				temp = foodBaseService.findAll(false);
			}
			else
			{
				temp = foodBaseService.findAny(filter);
			}

			Log.d(TAG, String.format("Searched for '%s', founded items: %d", filter, temp.size()));

			// sorter.sort(temp, mode == Mode.EDIT ? Sorter.Sort.ALPHABET : Sorter.Sort.RELEVANT);
			sorter.sort(temp, Sorter.Sort.RELEVANT);

			// TODO: check the performance
			List<Versioned<NamedRelativeTagged>> result = new ArrayList<Versioned<NamedRelativeTagged>>();
			for (Versioned<FoodItem> item : temp)
			{
				// Log.d(TAG, item.getData().getName() + " [" + item.getData().getTag() + "]");

				result.add(new Versioned<NamedRelativeTagged>(item));
			}

			tick = System.currentTimeMillis() - tick;
			Log.i(TAG, String.format("Request handled in %d msec, founded items: %d", tick, result.size()));

			return result;
		}
		catch (Exception e)
		{
			return null;
		}
	}

	void showBase(final List<Versioned<NamedRelativeTagged>> foodBase)
	{
		// TODO: localization
		if (foodBase == null)
		{
			UIUtils.showTip(this, "При загрузке данных произошла ошибка");
			showBase(Collections.<Versioned<NamedRelativeTagged>> emptyList());
			return;
		}

		data = foodBase;

		String[] str = new String[foodBase.size()];
		for (int i = 0; i < foodBase.size(); i++)
		{
			str[i] = foodBase.get(i).getData().getName();
		}

		setTitle(String.format("%s (%d)", getString(R.string.foodbase_title), foodBase.size()));

		ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_2,
				android.R.id.text1, str)
		{
			@Override
			public View getView(int position, View convertView, ViewGroup parent)
			{
				View view = super.getView(position, convertView, parent);
				TextView text1 = (TextView) view.findViewById(android.R.id.text1);
				TextView text2 = (TextView) view.findViewById(android.R.id.text2);

				text1.setText(foodBase.get(position).getData().getName());
				text2.setText(getInfo(foodBase.get(position).getData()));
				return view;
			}
		};

		listFood.setAdapter(adapter);
	}

	String getInfo(NamedRelativeTagged item)
	{
		String fmt = getString(R.string.foodbase_subinfo, item.getRelProts(), item.getRelFats(), item.getRelCarbs(),
				item.getRelValue());
		// fmt = fmt.replaceAll(" / ", "\t\t");
		// fmt = fmt + "\t\tTAG=" + item.getTag();
		return fmt;
	}

	void returnResult(String guid)
	{
		Intent intent = getIntent();
		intent.putExtra(KEY_GUID, guid);
		setResult(RESULT_OK, intent);
		finish();
	}

	void showFoodEditor(Versioned<FoodItem> food, boolean createMode)
	{
		Intent intent = new Intent(this, ActivityEditorFood.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, food);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);
		startActivityForResult(intent, createMode ? DIALOG_FOOD_CREATE : DIALOG_FOOD_MODIFY);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent intent)
	{
		super.onActivityResult(requestCode, resultCode, intent);

		try
		{
			switch (requestCode)
			{
				case DIALOG_FOOD_MODIFY:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<FoodItem> item = (Versioned<FoodItem>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						try
						{
							foodBaseService.save(Arrays.<Versioned<FoodItem>> asList(item));
							UIUtils.showTip(this, "Продукт сохранён");
						}
						catch (PersistenceException e)
						{
							// TODO: localize
							UIUtils.showTip(this, "Ошибка сохранения продукта");
						}
						runSearch(searchFilter);
					}
					break;
				}

				case DIALOG_FOOD_CREATE:
				{
					if (resultCode == RESULT_OK)
					{
						Versioned<FoodItem> item = (Versioned<FoodItem>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						try
						{
							foodBaseService.add(item);
							UIUtils.showTip(this, "Продукт создан");
						}
						catch (PersistenceException e)
						{
							// TODO: localize
							UIUtils.showTip(this, "Ошибка создания продукта");
						}
						runSearch(searchFilter);
					}
					break;
				}
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}
}
