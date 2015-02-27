package org.bosik.diacomp.android.frontend.fragments;

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
import org.bosik.diacomp.android.frontend.activities.ActivityEditor;
import org.bosik.diacomp.android.frontend.activities.ActivityEditorDish;
import org.bosik.diacomp.android.frontend.activities.ActivityEditorFood;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.search.Sorter;
import org.bosik.diacomp.core.services.search.Sorter.Sort;
import android.app.Activity;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.util.SparseBooleanArray;
import android.view.ActionMode;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AbsListView.MultiChoiceModeListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.BaseAdapter;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

public class FragmentBase extends Fragment
{
	private static final String							TAG					= FragmentBase.class.getSimpleName();

	private static final int							DIALOG_FOOD_CREATE	= 11;
	private static final int							DIALOG_FOOD_MODIFY	= 12;
	private static final int							DIALOG_DISH_CREATE	= 21;
	private static final int							DIALOG_DISH_MODIFY	= 22;

	private static final int							LIMIT				= 100;

	// Widgets
	EditText											editSearch;
	private ListView									list;

	// Data
	final FoodBaseService								foodBaseService		= Storage.localFoodBase;
	final DishBaseService								dishBaseService		= Storage.localDishBase;
	private final Map<String, Integer>					tagInfo				= Storage.tagService.getTags();
	List<Versioned<NamedRelativeTagged>>				data				= new ArrayList<Versioned<NamedRelativeTagged>>();
	BaseAdapter											adapter;
	private static final Sorter<NamedRelativeTagged>	sorter				= new Sorter<NamedRelativeTagged>();
	String												searchFilter		= "";
	boolean												resultCutted;

	long												lastSearchTime;
	boolean												searchScheduled		= false;
	private static final long							SEARCH_DELAY		= 500;

	@Override
	public View onCreateView(final LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		setHasOptionsMenu(true);

		View rootView = inflater.inflate(R.layout.fragment_base, container, false);

		// Widgets binding
		editSearch = (EditText) rootView.findViewById(R.id.editBaseEditorSearch);
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
		list = (ListView) rootView.findViewById(R.id.listBaseEditorSearchResults);
		list.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE_MODAL);
		list.setMultiChoiceModeListener(new MultiChoiceModeListener()
		{
			@Override
			public void onItemCheckedStateChanged(ActionMode actionMode, int i, long l, boolean b)
			{
				int selectedCount = list.getCheckedItemCount();
				setSubtitle(actionMode, selectedCount);
			}

			@Override
			public boolean onCreateActionMode(ActionMode actionMode, Menu menu)
			{
				MenuInflater inflater = actionMode.getMenuInflater();
				inflater.inflate(R.menu.actions_base_context, menu);
				return true;
			}

			@Override
			public boolean onPrepareActionMode(ActionMode actionMode, Menu menu)
			{
				return false;
			}

			@Override
			public boolean onActionItemClicked(ActionMode actionMode, MenuItem menuItem)
			{
				List<Versioned<FoodItem>> removedFoods = new ArrayList<Versioned<FoodItem>>();
				List<Versioned<DishItem>> removedDishes = new ArrayList<Versioned<DishItem>>();

				SparseBooleanArray checkList = list.getCheckedItemPositions();
				for (int i = 0; i < checkList.size(); i++)
				{
					if (checkList.valueAt(i))
					{
						int index = checkList.keyAt(i);
						if (index >= 0 && index < data.size())
						{
							Versioned<NamedRelativeTagged> item = data.get(index);

							if (item.getData() instanceof FoodItem)
							{
								item.setDeleted(true);
								removedFoods.add(new Versioned<FoodItem>(item));
							}
							else if (item.getData() instanceof DishItem)
							{
								item.setDeleted(true);
								removedDishes.add(new Versioned<DishItem>(item));
							}
						}
					}
				}

				foodBaseService.save(removedFoods);
				dishBaseService.save(removedDishes);
				runSearch();

				// TODO: i18n
				String text = (removedFoods.size() + removedDishes.size()) + " items removed";
				Toast.makeText(list.getContext(), text, Toast.LENGTH_LONG).show();

				return true;
			}

			@Override
			public void onDestroyActionMode(ActionMode actionMode)
			{
			}

			private void setSubtitle(ActionMode mode, int selectedCount)
			{
				switch (selectedCount)
				{
					case 0:
						mode.setSubtitle(null);
						break;
					default:
						mode.setTitle(String.valueOf(selectedCount));
						break;
				}
			}
		});
		list.setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long itemIndex)
			{
				final String id = data.get(position).getId();

				new AsyncTask<String, Void, Versioned<? extends NamedRelativeTagged>>()
				{
					@Override
					protected void onPreExecute()
					{
						// setTitle(getString(R.string.base_title_loading));
					}

					@Override
					protected Versioned<? extends NamedRelativeTagged> doInBackground(String... params)
					{
						Versioned<? extends NamedRelativeTagged> food = foodBaseService.findById(id);
						if (food != null)
						{
							return food;
						}

						Versioned<? extends NamedRelativeTagged> dish = dishBaseService.findById(id);
						if (dish != null)
						{
							return dish;
						}

						return null;
					}

					@SuppressWarnings("unchecked")
					@Override
					protected void onPostExecute(Versioned<? extends NamedRelativeTagged> item)
					{
						if (item != null)
						{
							if (item.getData().getClass().isAssignableFrom(FoodItem.class))
							{
								showFoodEditor((Versioned<FoodItem>) item, false);
							}
							else if (item.getData().getClass().isAssignableFrom(DishItem.class))
							{
								showDishEditor((Versioned<DishItem>) item, false);
							}
							else
							{
								// TODO: i18n
								UIUtils.showTip(getActivity(), String.format("Unknown record type (ID: %s)", id));
							}
						}
						else
						{
							// TODO: i18n
							UIUtils.showTip(getActivity(), String.format("Item %s not found", id));
						}
					}
				}.execute(id);
			}
		});

		adapter = new BaseAdapter()
		{
			static final int	TYPE_FOOD	= 1;
			static final int	TYPE_DISH	= 2;
			static final int	UNKNOWN		= -17;

			@Override
			public int getViewTypeCount()
			{
				return 3;
			}

			@Override
			public int getItemViewType(int position)
			{
				final Versioned<?> item = (Versioned<?>) getItem(position);
				final Object itemData = item.getData();

				if (itemData instanceof FoodItem)
				{
					return TYPE_FOOD;
				}
				else if (itemData instanceof DishItem)
				{
					return TYPE_DISH;
				}
				else
				{
					return UNKNOWN;
				}
			}

			@Override
			public int getCount()
			{
				return data.size();
			}

			@Override
			public Object getItem(int position)
			{
				synchronized (data)
				{
					if (position >= 0 && position < data.size())
					{
						return data.get(position);
					}
					else
					{
						return null;
					}
				}
			}

			@Override
			public boolean hasStableIds()
			{
				return true;
			}

			@Override
			public long getItemId(int position)
			{
				Object item = getItem(position);
				if (item != null)
				{
					return ((Versioned<?>) item).getId().hashCode();
				}
				else
				{
					return position;
				}
			}

			@Override
			public View getView(int position, View convertView, ViewGroup parent)
			{
				Versioned<?> item = (Versioned<?>) getItem(position);

				if (item == null)
				{
					if (convertView == null)
					{
						// FIXME
						convertView = inflater.inflate(R.layout.view_diary_rec_loading, null);
					}
				}
				else
				{
					switch (getItemViewType(position))
					{
						case TYPE_FOOD:
						{
							if (convertView == null)
							{
								convertView = inflater.inflate(R.layout.view_base_food, null);
							}

							final FoodItem food = (FoodItem) item.getData();

							TextView textName = (TextView) convertView.findViewById(R.id.baseItemFoodName);
							textName.setText(food.getName());
							TextView textInfo = (TextView) convertView.findViewById(R.id.baseItemFoodInfo);
							textInfo.setText(getInfo(food));
							break;
						}
						case TYPE_DISH:
						{
							// TODO: use dish-specific layout
							if (convertView == null)
							{
								convertView = inflater.inflate(R.layout.view_base_food, null);
							}

							final DishItem dish = (DishItem) item.getData();

							TextView textName = (TextView) convertView.findViewById(R.id.baseItemFoodName);
							textName.setText(dish.getName());
							TextView textInfo = (TextView) convertView.findViewById(R.id.baseItemFoodInfo);
							textInfo.setText(getInfo(dish));
							break;
						}
						default:
						{
							throw new RuntimeException("Invalid data type: " + item);
						}
					}
				}

				convertView.setBackgroundDrawable(getResources().getDrawable(R.drawable.background_base_item));
				return convertView;
			}
		};

		list.setAdapter(adapter);
		runSearch();

		return rootView;
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
				// getActivity().setTitle(getString(R.string.base_title_loading));
			}

			@Override
			protected List<Versioned<NamedRelativeTagged>> doInBackground(String... params)
			{
				return request(params[0]);
			}

			@Override
			protected void onPostExecute(List<Versioned<NamedRelativeTagged>> result)
			{
				// TODO: i18n
				if (result == null)
				{
					UIUtils.showTip(getActivity(), "При загрузке данных произошла ошибка");
					result = Collections.<Versioned<NamedRelativeTagged>> emptyList();
				}

				synchronized (data)
				{
					data = result;
				}
				adapter.notifyDataSetChanged();
				searchScheduled = false;
			}
		};

		TimerTask task = new TimerTask()
		{
			@Override
			public void run()
			{
				asyncTask.execute(searchFilter);
			}
		};

		if ((System.currentTimeMillis() - lastSearchTime) >= SEARCH_DELAY)
		{
			lastSearchTime = System.currentTimeMillis();
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

			List<Versioned<FoodItem>> foodItems;
			List<Versioned<DishItem>> dishItems;

			if (filter.trim().isEmpty())
			{
				foodItems = foodBaseService.findAll(false);
				dishItems = dishBaseService.findAll(false);
			}
			else
			{
				foodItems = foodBaseService.findAny(filter);
				dishItems = dishBaseService.findAny(filter);
			}

			// indexing
			Map<String, Versioned<? extends NamedRelativeTagged>> baseIndex = new HashMap<String, Versioned<? extends NamedRelativeTagged>>();
			for (Versioned<? extends NamedRelativeTagged> item : foodItems)
			{
				baseIndex.put(item.getId(), item);
			}
			for (Versioned<? extends NamedRelativeTagged> item : dishItems)
			{
				baseIndex.put(item.getId(), item);
			}

			Log.d(TAG, String.format("Searched for '%s', founded items: %d", filter, baseIndex.size()));

			// sorter.sort(temp, mode == Mode.EDIT ? Sorter.Sort.ALPHABET : Sorter.Sort.RELEVANT);
			// sorter.sort(temp, Sorter.Sort.RELEVANT);

			// TODO: check the performance
			List<Versioned<NamedRelativeTagged>> result = new ArrayList<Versioned<NamedRelativeTagged>>();

			for (Entry<String, Integer> tag : tagInfo.entrySet())
			{
				Versioned<? extends NamedRelativeTagged> indexItem = baseIndex.get(tag.getKey());
				if (indexItem != null)
				{
					Versioned<NamedRelativeTagged> item = new Versioned<NamedRelativeTagged>(indexItem);
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
				resultCutted = true;
				if (result.size() < LIMIT)
				{
					int i = 0;
					int j = 0;

					while (result.size() < LIMIT)
					{
						while ((i < foodItems.size()) && (tagInfo.get(foodItems.get(i).getId()) != null))
						{
							i++;
						}
						while ((j < dishItems.size()) && (tagInfo.get(dishItems.get(j).getId()) != null))
						{
							j++;
						}

						if ((i < foodItems.size()) && (j < dishItems.size()))
						{
							// TODO: check the more/less sign
							if (foodItems.get(i).getData().getName().compareTo(dishItems.get(j).getData().getName()) < 0)
							{
								result.add(new Versioned<NamedRelativeTagged>(foodItems.get(i++)));
							}
							else
							{
								result.add(new Versioned<NamedRelativeTagged>(dishItems.get(j++)));
							}
						}
						else if (i < foodItems.size())
						{
							result.add(new Versioned<NamedRelativeTagged>(foodItems.get(i++)));
						}
						else if (j < dishItems.size())
						{
							result.add(new Versioned<NamedRelativeTagged>(dishItems.get(j++)));
						}
						else
						{
							// no more elements are available
							resultCutted = false;
							break;
						}
					}

					// for (int i = 0; i < foodItems.size(); i++)
					// {
					// if (tagInfo.get(foodItems.get(i).getId()) == null)
					// {
					// result.add(new Versioned<NamedRelativeTagged>(foodItems.get(i)));
					// if (result.size() == LIMIT)
					// {
					// resultCutted = (i < foodItems.size() - 1);
					// break;
					// }
					// }
					// }
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

	String getInfo(NamedRelativeTagged item)
	{
		String fmt = getString(R.string.base_subinfo, item.getRelProts(), item.getRelFats(), item.getRelCarbs(),
				item.getRelValue());
		// fmt = fmt.replaceAll(" / ", "\t\t");
		// fmt = fmt + "\t\tTAG=" + item.getTag();
		return fmt;
	}

	void showFoodEditor(Versioned<FoodItem> food, boolean createMode)
	{
		Intent intent = new Intent(getActivity(), ActivityEditorFood.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, food);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);
		startActivityForResult(intent, createMode ? DIALOG_FOOD_CREATE : DIALOG_FOOD_MODIFY);
	}

	void showDishEditor(Versioned<DishItem> dish, boolean createMode)
	{
		Intent intent = new Intent(getActivity(), ActivityEditorDish.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, dish);
		intent.putExtra(ActivityEditor.FIELD_MODE, createMode);
		startActivityForResult(intent, createMode ? DIALOG_DISH_CREATE : DIALOG_DISH_MODIFY);
	}

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
	{
		inflater.inflate(R.menu.actions_base, menu);
		super.onCreateOptionsMenu(menu, inflater);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		switch (item.getItemId())
		{
			case R.id.item_base_addFood:
			{
				FoodItem food = new FoodItem();
				food.setName(editSearch.getText().toString());
				showFoodEditor(new Versioned<FoodItem>(food), true);
				return true;
			}
			case R.id.item_base_addDish:
			{
				DishItem dish = new DishItem();
				dish.setName(editSearch.getText().toString());
				showDishEditor(new Versioned<DishItem>(dish), true);
				return true;
			}
			default:
			{
				return false;// super.onOptionsItemSelected(item);
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent intent)
	{
		try
		{
			switch (requestCode)
			{
				case DIALOG_FOOD_CREATE:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<FoodItem> item = (Versioned<FoodItem>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						try
						{
							foodBaseService.add(item);
							// TODO: i18n
							UIUtils.showTip(getActivity(), "Продукт создан");
						}
						catch (PersistenceException e)
						{
							// TODO: i18n
							UIUtils.showTip(getActivity(), "Ошибка создания продукта");
						}
						runSearch();
					}
					break;
				}

				case DIALOG_FOOD_MODIFY:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<FoodItem> item = (Versioned<FoodItem>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						try
						{
							// TODO: i18n
							foodBaseService.save(Arrays.<Versioned<FoodItem>> asList(item));
							UIUtils.showTip(getActivity(), "Продукт сохранён");
						}
						catch (PersistenceException e)
						{
							// TODO: i18n
							UIUtils.showTip(getActivity(), "Ошибка сохранения продукта");
						}
						runSearch();
					}
					break;
				}

				case DIALOG_DISH_CREATE:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<DishItem> item = (Versioned<DishItem>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						try
						{
							dishBaseService.add(item);
							// TODO: i18n
							UIUtils.showTip(getActivity(), "Блюдо создано");
						}
						catch (PersistenceException e)
						{
							// TODO: i18n
							UIUtils.showTip(getActivity(), "Ошибка создания блюда");
						}
						runSearch();
					}
					break;
				}

				case DIALOG_DISH_MODIFY:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<DishItem> item = (Versioned<DishItem>) intent.getExtras().getSerializable(
								ActivityEditor.FIELD_ENTITY);
						try
						{
							dishBaseService.save(Arrays.<Versioned<DishItem>> asList(item));
							// TODO: i18n
							UIUtils.showTip(getActivity(), "Блюдо сохранено");
						}
						catch (PersistenceException e)
						{
							// TODO: i18n
							UIUtils.showTip(getActivity(), "Ошибка сохранения блюда");
						}
						runSearch();
					}
					break;
				}
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, getActivity());
		}
	}
}