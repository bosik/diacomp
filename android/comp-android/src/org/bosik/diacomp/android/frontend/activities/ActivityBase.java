package org.bosik.diacomp.android.frontend.activities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Timer;
import java.util.TimerTask;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.dishbase.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.core.services.search.Sorter;
import org.bosik.diacomp.core.services.search.Sorter.Sort;
import android.app.Activity;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
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

public class ActivityBase extends Activity
{
	private static final String	TAG					= ActivityBase.class.getSimpleName();

	public static final String	KEY_GUID			= "diacomp.activitybase.guid";
	public static final String	KEY_MODE			= "diacomp.activitybase.mode";
	// TODO: Pick mode seems to be useless
	public static final String	VALUE_MODE_PICK		= "diacomp.activitybase.mode.pick";
	public static final String	VALUE_MODE_EDIT		= "diacomp.activitybase.mode.edit";

	private static final int	DIALOG_FOOD_CREATE	= 11;
	private static final int	DIALOG_FOOD_MODIFY	= 12;

	private static final int	LIMIT				= 100;

	private enum Mode
	{
		EDIT, PICK
	}

	// Widgets
	private EditText									editSearch;
	private ListView									list;
	private Button										buttonFoodCreate;
	private Button										buttonDishCreate;

	// Data
	final FoodBaseService								foodBaseService	= Storage.localFoodBase;
	final DishBaseService								dishBaseService	= Storage.localDishBase;
	private final Map<String, Integer>					tagInfo			= Storage.tagService.getTags();
	List<Versioned<NamedRelativeTagged>>				data;
	private static final Sorter<NamedRelativeTagged>	sorter			= new Sorter<NamedRelativeTagged>();
	Mode												mode;
	String												searchFilter	= "";
	boolean												resultCutted;

	long												lastSearchTime;
	boolean												searchScheduled	= false;
	private static final long							SEARCH_DELAY	= 500;

	// ===========================================================================

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_base);

		// reading intent
		Intent intent = getIntent();
		mode = VALUE_MODE_PICK.equals(intent.getStringExtra(KEY_MODE)) ? Mode.PICK : Mode.EDIT;

		// Widgets binding
		editSearch = (EditText) findViewById(R.id.editBaseEditorSearch);
		editSearch.addTextChangedListener(new TextWatcher()
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
				runSearch();
			}
		});
		list = (ListView) findViewById(R.id.listBaseEditorSearchResults);
		list.setOnItemClickListener(new OnItemClickListener()
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
									UIUtils.showTip(ActivityBase.this, String.format("Item %s not found", guid));
								}
							}
						}.execute(guid);

						// TODO: do the same for dish base when ready
					}
				}

			}
		});

		buttonFoodCreate = (Button) findViewById(R.id.buttonBaseEditorCreateFood);
		buttonFoodCreate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View arg0)
			{
				showFoodEditor(new Versioned<FoodItem>(new FoodItem()), true);
			}
		});
		buttonDishCreate = (Button) findViewById(R.id.buttonBaseEditorCreateDish);
		buttonDishCreate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View arg0)
			{
				showDishEditor(new Versioned<DishItem>(new DishItem()), true);
			}
		});

		// Show data
		runSearch();
	}

	/**
	 * Runs search process in the background thread, fills result list when done
	 * 
	 */
	void runSearch()
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
				return request(searchFilter/* params[0] */);
			}

			@Override
			protected void onPostExecute(List<Versioned<NamedRelativeTagged>> result)
			{
				showBase(result);
			}
		};

		TimerTask task = new TimerTask()
		{
			private final Handler	mHandler	= new Handler(Looper.getMainLooper());

			@Override
			public void run()
			{
				mHandler.post(new Runnable()
				{
					@Override
					public void run()
					{
						lastSearchTime = System.currentTimeMillis();
						asyncTask.execute(/* key */);
						searchScheduled = false;
					}
				});
			}
		};

		if ((System.currentTimeMillis() - lastSearchTime) >= SEARCH_DELAY)
		{
			task.run();
		}
		else
		{
			if (!searchScheduled)
			{
				searchScheduled = true;
				new Timer().schedule(task, SEARCH_DELAY - (System.currentTimeMillis() - lastSearchTime));
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

			// indexing
			Map<String, Versioned<FoodItem>> foodBaseIndex = new HashMap<String, Versioned<FoodItem>>();
			for (Versioned<FoodItem> item : temp)
			{
				foodBaseIndex.put(item.getId(), item);
			}

			Log.d(TAG, String.format("Searched for '%s', founded items: %d", filter, temp.size()));

			// sorter.sort(temp, mode == Mode.EDIT ? Sorter.Sort.ALPHABET : Sorter.Sort.RELEVANT);
			// sorter.sort(temp, Sorter.Sort.RELEVANT);

			// TODO: check the performance
			List<Versioned<NamedRelativeTagged>> result = new ArrayList<Versioned<NamedRelativeTagged>>();

			for (Entry<String, Integer> tag : tagInfo.entrySet())
			{
				Versioned<FoodItem> food = foodBaseIndex.get(tag.getKey());
				if (food != null)
				{
					Versioned<NamedRelativeTagged> item = new Versioned<NamedRelativeTagged>(food);
					item.getData().setTag(tag.getValue());
					result.add(item);
				}
			}

			sorter.sort(result, Sort.RELEVANT);

			if (result.size() > LIMIT)
			{
				resultCutted = true;
				result = result.subList(0, LIMIT);
			}
			else
			{
				resultCutted = false;
				if (result.size() < LIMIT)
				{
					for (int i = 0; i < temp.size(); i++)
					{
						if (tagInfo.get(temp.get(i).getId()) == null)
						{
							result.add(new Versioned<NamedRelativeTagged>(temp.get(i)));
							if (result.size() == LIMIT)
							{
								resultCutted = (i < temp.size() - 1);
								break;
							}
						}
					}
				}
			}

			// for (Versioned<FoodItem> item : temp)
			// {
			// // Log.d(TAG, item.getData().getName() + " [" + item.getData().getTag() + "]");
			// result.add(new Versioned<NamedRelativeTagged>(item));
			// }

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

		String fmt = resultCutted ? "%s (%d+)" : "%s (%d)";
		setTitle(String.format(fmt, getString(R.string.foodbase_title), foodBase.size()));

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

		list.setAdapter(adapter);
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

	void showDishEditor(Versioned<DishItem> dish, boolean createMode)
	{
		Intent intent = new Intent(this, ActivityEditorDish.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, dish);
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
						runSearch();
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
						runSearch();
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
