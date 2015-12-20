/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.frontend.fragments;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.AccountUtils;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.android.backend.features.dishbase.LocalDishBase;
import org.bosik.diacomp.android.backend.features.foodbase.LocalFoodBase;
import org.bosik.diacomp.android.backend.features.search.TagServiceInternal;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.activities.ActivityEditor;
import org.bosik.diacomp.android.frontend.activities.ActivityEditorDish;
import org.bosik.diacomp.android.frontend.activities.ActivityEditorFood;
import org.bosik.diacomp.android.frontend.activities.ActivityFoodSet;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.search.Sorter;
import org.bosik.diacomp.core.services.search.Sorter.Sort;
import org.bosik.diacomp.core.services.search.TagService;
import org.bosik.merklesync.Versioned;
import android.app.Activity;
import android.content.ContentResolver;
import android.content.Intent;
import android.database.ContentObserver;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Binder;
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
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AbsListView.MultiChoiceModeListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

public class FragmentTabBase extends Fragment
{
	private static final String						TAG					= FragmentTabBase.class.getSimpleName();

	private static final int						DIALOG_FOOD_CREATE	= 11;
	private static final int						DIALOG_FOOD_MODIFY	= 12;
	private static final int						DIALOG_DISH_CREATE	= 21;
	private static final int						DIALOG_DISH_MODIFY	= 22;

	private static final int						LIMIT				= 100;

	// Widgets
	View											groupBaseEmpty;
	View											groupBaseContent;
	Button											buttonFoodSets;
	EditText										editSearch;
	ListView										list;

	// Data
	FoodBaseService									foodBaseService;
	DishBaseService									dishBaseService;
	private TagService								tagService;
	List<Versioned<? extends NamedRelativeTagged>>	data				= new ArrayList<Versioned<? extends NamedRelativeTagged>>();
	BaseAdapter										adapter;
	private static final Sorter						sorter				= new Sorter();
	String											searchFilter		= "";
	boolean											resultCutted;

	long											lastSearchTime;
	boolean											searchScheduled		= false;
	private static final long						SEARCH_DELAY		= 500;

	private ContentObserver							observer			= new ContentObserver(null)
																		{
																			@Override
																			public void onChange(boolean selfChange)
																			{
																				this.onChange(selfChange, null);
																			}

																			@Override
																			public void onChange(boolean selfChange,
																					Uri uri)
																			{
																				if (uri != null)
																				{
																					switch (DiaryContentProvider.sURIMatcher
																							.match(uri))
																					{
																						case DiaryContentProvider.CODE_FOODBASE:
																						case DiaryContentProvider.CODE_DISHBASE:
																						{
																							final long token = Binder
																									.clearCallingIdentity();
																							try
																							{
																								checkEmptiness();
																							}
																							finally
																							{
																								Binder.restoreCallingIdentity(
																										token);
																							}
																							runSearch();
																							break;
																						}
																					}
																				}
																			}
																		};

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		ContentResolver resolver = getActivity().getContentResolver();

		resolver.registerContentObserver(DiaryContentProvider.CONTENT_BASE_URI, true, observer);
		foodBaseService = LocalFoodBase.getInstance(resolver);
		dishBaseService = LocalDishBase.getInstance(resolver);
		tagService = TagServiceInternal.getInstance();
	}

	@Override
	public void onDestroy()
	{
		super.onDestroy();
		getActivity().getContentResolver().unregisterContentObserver(observer);
	}

	@Override
	public View onCreateView(final LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		setHasOptionsMenu(true);

		View rootView = inflater.inflate(R.layout.fragment_tab_base, container, false);

		// Widgets binding
		groupBaseEmpty = rootView.findViewById(R.id.layoutBaseEmpty);
		groupBaseContent = rootView.findViewById(R.id.layoutBaseContent);
		buttonFoodSets = (Button) rootView.findViewById(R.id.buttonBaseFoodSets);
		buttonFoodSets.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				Intent intent = new Intent(getActivity(), ActivityFoodSet.class);
				startActivity(intent);
			}
		});
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
							Versioned<? extends NamedRelativeTagged> item = data.get(index);

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

				// TODO: move outside UI thread
				foodBaseService.save(removedFoods);
				dishBaseService.save(removedDishes);
				runSearch();

				int count = removedFoods.size() + removedDishes.size();
				String text = String.format(getString(R.string.base_tip_items_removed), count);
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
							if (convertView == null)
							{
								convertView = inflater.inflate(R.layout.view_base_dish, null);
							}

							final DishItem dish = (DishItem) item.getData();

							TextView textName = (TextView) convertView.findViewById(R.id.baseItemDishName);
							textName.setText(dish.getName());
							TextView textInfo = (TextView) convertView.findViewById(R.id.baseItemDishInfo);
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
		checkEmptiness();
		runSearch();

		return rootView;
	}

	/**
	 * Runs search process in the background thread, fills result list when done
	 * 
	 */
	synchronized void runSearch()
	{
		final AsyncTask<String, Void, List<Versioned<? extends NamedRelativeTagged>>> asyncTask = new AsyncTask<String, Void, List<Versioned<? extends NamedRelativeTagged>>>()
		{
			@Override
			protected void onPreExecute()
			{
				// getActivity().setTitle(getString(R.string.base_title_loading));
			}

			@Override
			protected List<Versioned<? extends NamedRelativeTagged>> doInBackground(String... params)
			{
				return request(params[0]);
			}

			@Override
			protected void onPostExecute(List<Versioned<? extends NamedRelativeTagged>> result)
			{
				// TODO: i18n
				if (result == null)
				{
					UIUtils.showTip(getActivity(), "При загрузке данных произошла ошибка");
					result = Collections.<Versioned<? extends NamedRelativeTagged>> emptyList();
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
	List<Versioned<? extends NamedRelativeTagged>> request(String filter)
	{
		try
		{
			long tick = System.currentTimeMillis();

			// filtering

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

			// relevance ordering

			List<Versioned<? extends NamedRelativeTagged>> result = new ArrayList<Versioned<? extends NamedRelativeTagged>>();

			for (Versioned<? extends NamedRelativeTagged> item : foodItems)
			{
				Integer tag = tagService.getTag(item.getId());
				item.getData().setTag(tag != null ? tag : 0);
				result.add(item);
			}
			for (Versioned<? extends NamedRelativeTagged> item : dishItems)
			{
				Integer tag = tagService.getTag(item.getId());
				item.getData().setTag(tag != null ? tag : 0);
				result.add(item);
			}

			Log.d(TAG, String.format("Searched for '%s', items found: %d", filter, result.size()));

			sorter.sort(result, Sort.RELEVANT);

			// clipping

			if (result.size() > LIMIT)
			{
				resultCutted = true;
				result = result.subList(0, LIMIT);
			}
			else
			{
				resultCutted = false;
			}

			tick = System.currentTimeMillis() - tick;
			Log.i(TAG, String.format("Request handled in %d msec, items found: %d", tick, result.size()));

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
		super.onCreateOptionsMenu(menu, inflater);
		inflater.inflate(R.menu.actions_base, menu);

		if (AccountUtils.getAccounts(getActivity()).length > 0)
		{
			MenuItem item = menu.findItem(R.id.item_common_login);
			if (item != null)
			{
				item.setVisible(false);
			}
		}
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
			case R.id.item_base_foodsets:
			{
				Intent intent = new Intent(getActivity(), ActivityFoodSet.class);
				startActivity(intent);
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
						Versioned<FoodItem> item = (Versioned<FoodItem>) intent.getExtras()
								.getSerializable(ActivityEditor.FIELD_ENTITY);
						try
						{
							foodBaseService.add(item);
							UIUtils.showTip(getActivity(), getString(R.string.base_tip_food_create_ok));
						}
						catch (PersistenceException e)
						{
							UIUtils.showTip(getActivity(), getString(R.string.base_tip_food_create_fail));
						}
						runSearch();
					}
					break;
				}

				case DIALOG_FOOD_MODIFY:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<FoodItem> item = (Versioned<FoodItem>) intent.getExtras()
								.getSerializable(ActivityEditor.FIELD_ENTITY);
						try
						{
							foodBaseService.save(Arrays.<Versioned<FoodItem>> asList(item));
							UIUtils.showTip(getActivity(), getString(R.string.base_tip_food_edit_ok));
						}
						catch (PersistenceException e)
						{
							UIUtils.showTip(getActivity(), getString(R.string.base_tip_food_edit_fail));
						}
						runSearch();
					}
					break;
				}

				case DIALOG_DISH_CREATE:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<DishItem> item = (Versioned<DishItem>) intent.getExtras()
								.getSerializable(ActivityEditor.FIELD_ENTITY);
						try
						{
							dishBaseService.add(item);
							UIUtils.showTip(getActivity(), getString(R.string.base_tip_dish_create_ok));
						}
						catch (PersistenceException e)
						{
							UIUtils.showTip(getActivity(), getString(R.string.base_tip_dish_create_fail));
						}
						runSearch();
					}
					break;
				}

				case DIALOG_DISH_MODIFY:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<DishItem> item = (Versioned<DishItem>) intent.getExtras()
								.getSerializable(ActivityEditor.FIELD_ENTITY);
						try
						{
							dishBaseService.save(Arrays.<Versioned<DishItem>> asList(item));
							UIUtils.showTip(getActivity(), getString(R.string.base_tip_dish_save_ok));
						}
						catch (PersistenceException e)
						{
							UIUtils.showTip(getActivity(), getString(R.string.base_tip_dish_save_fail));
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

	void checkEmptiness()
	{
		// int total = foodBaseService.count("") + dishBaseService.count("");
		//
		// if (total > 0)
		// {
		groupBaseEmpty.setVisibility(View.GONE);
		groupBaseContent.setVisibility(View.VISIBLE);
		// }
		// else
		// {
		// groupBaseEmpty.setVisibility(View.VISIBLE);
		// groupBaseContent.setVisibility(View.GONE);
		// }
	}
}