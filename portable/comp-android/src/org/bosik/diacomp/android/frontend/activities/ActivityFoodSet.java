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
package org.bosik.diacomp.android.frontend.activities;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.backend.features.foodset.FoodSetService;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.FoodSetInfo;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.services.preferences.Preference;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.merklesync.Versioned;
import android.content.Context;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.FragmentActivity;
import android.util.SparseBooleanArray;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.ArrayAdapter;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.CheckedTextView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;

public class ActivityFoodSet extends FragmentActivity
{
	public static final String	FIELD_FIRST_START	= "bosik.pack.firstStart";

	// UI components

	TextView					labelHint;
	ProgressBar					progressBar;
	ListView					listFoodSets;
	Button						buttonOk;

	// Data

	BaseAdapter					adapter;
	List<FoodSetInfo>			data;
	PreferencesTypedService		syncablePreferences;
	Set<String>					includedFoodSets;
	boolean						firstStart;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		// basic setup

		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_food_set);

		// data services setup

		syncablePreferences = new PreferencesTypedService(new PreferencesLocalService(getContentResolver()));
		includedFoodSets = syncablePreferences.getStringSet(Preference.FOOD_SETS);
		firstStart = getIntent().getBooleanExtra(FIELD_FIRST_START, false);

		// UI setup
		labelHint = (TextView) findViewById(R.id.labelFoodSetsHint);
		progressBar = (ProgressBar) findViewById(R.id.progressBarFoodSets);
		listFoodSets = (ListView) findViewById(R.id.listFoodSets);
		listFoodSets.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);

		// adapter = new BaseAdapter()
		// {
		// static final int TYPE_FOOD = 1;
		// static final int TYPE_DISH = 2;
		// static final int UNKNOWN = -17;
		//
		// @Override
		// public int getViewTypeCount()
		// {
		// return 1;
		// }
		//
		// @Override
		// public int getCount()
		// {
		// return data.size();
		// }
		//
		// @Override
		// public Object getItem(int position)
		// {
		// synchronized (data)
		// {
		// if (position >= 0 && position < data.size())
		// {
		// return data.get(position);
		// }
		// else
		// {
		// return null;
		// }
		// }
		// }
		//
		// @Override
		// public View getView(int position, View convertView, ViewGroup parent)
		// {
		// Versioned<?> item = (Versioned<?>) getItem(position);
		//
		// if (item == null)
		// {
		// if (convertView == null)
		// {
		// // FIXME
		// convertView = inflater.inflate(R.layout.view_diary_rec_loading, null);
		// }
		// }
		// else
		// {
		// switch (getItemViewType(position))
		// {
		// case TYPE_FOOD:
		// {
		// if (convertView == null)
		// {
		// convertView = inflater.inflate(R.layout.view_base_food, null);
		// }
		//
		// final FoodItem food = (FoodItem) item.getData();
		//
		// TextView textName = (TextView) convertView.findViewById(R.id.baseItemFoodName);
		// textName.setText(food.getName());
		// TextView textInfo = (TextView) convertView.findViewById(R.id.baseItemFoodInfo);
		// textInfo.setText(getInfo(food));
		// break;
		// }
		// case TYPE_DISH:
		// {
		// if (convertView == null)
		// {
		// convertView = inflater.inflate(R.layout.view_base_dish, null);
		// }
		//
		// final DishItem dish = (DishItem) item.getData();
		//
		// TextView textName = (TextView) convertView.findViewById(R.id.baseItemDishName);
		// textName.setText(dish.getName());
		// TextView textInfo = (TextView) convertView.findViewById(R.id.baseItemDishInfo);
		// textInfo.setText(getInfo(dish));
		// break;
		// }
		// default:
		// {
		// throw new RuntimeException("Invalid data type: " + item);
		// }
		// }
		// }
		//
		// convertView.setBackgroundDrawable(getResources().getDrawable(R.drawable.background_base_item));
		// return convertView;
		// }
		// };
		//
		// list.setAdapter(adapter);

		data = new ArrayList<FoodSetInfo>();
		adapter = new ArrayAdapter<FoodSetInfo>(this, android.R.layout.simple_list_item_checked, data)
		{
			@Override
			public int getCount()
			{
				return data.size();
			}

			@Override
			public View getView(int position, View convertView, ViewGroup parent)
			{
				if (convertView == null)
				{
					LayoutInflater inflater = (LayoutInflater) ActivityFoodSet.this
							.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
					convertView = inflater.inflate(android.R.layout.simple_list_item_checked, parent, false);
				}

				FoodSetInfo foodSetInfo = data.get(position);
				CheckedTextView textView = (CheckedTextView) convertView.findViewById(android.R.id.text1);
				String text = String.format("%s (%d)", foodSetInfo.getDescription(), foodSetInfo.getSize());
				textView.setText(text);
				return convertView;
			}
		};
		listFoodSets.setAdapter(adapter);

		buttonOk = (Button) findViewById(R.id.buttonFoodSetsOk);
		buttonOk.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				SparseBooleanArray checkList = listFoodSets.getCheckedItemPositions();
				for (int i = 0; i < data.size(); i++)
				{
					boolean oldIncluded = includedFoodSets.contains(data.get(i).getId());
					boolean newIncluded = checkList.get(i);

					if (oldIncluded != newIncluded)
					{
						updateSet(data.get(i), newIncluded);
					}
				}

				finish();
			}
		});

		// =================================================================================

		/**
		 * Loading food sets
		 */
		new AsyncTask<Void, Void, List<FoodSetInfo>>()
		{
			@Override
			protected void onPreExecute()
			{
				labelHint.setText(getString(R.string.foodset_hint_loading));
				progressBar.setVisibility(View.VISIBLE);
				buttonOk.setVisibility(View.GONE);
			};

			@Override
			protected List<FoodSetInfo> doInBackground(Void... params)
			{
				try
				{
					FoodSetService foodSetService = new FoodSetService(Storage.webClient);
					return foodSetService.getFoodSets();
				}
				catch (Exception e)
				{
					return null;
				}
			}

			@Override
			protected void onPostExecute(List<FoodSetInfo> data)
			{
				progressBar.setVisibility(View.GONE);

				if (data != null)
				{
					labelHint.setText(getString(R.string.foodset_hint_ok));

					ActivityFoodSet.this.data = data;
					adapter.notifyDataSetChanged();

					for (int i = 0; i < data.size(); i++)
					{
						String id = data.get(i).getId();
						boolean checked = firstStart || includedFoodSets.contains(id);
						listFoodSets.setItemChecked(i, checked);
					}

					buttonOk.setVisibility(View.VISIBLE);
				}
				else
				{
					labelHint.setText(getString(R.string.foodset_hint_failed));
				}
			}
		}.execute();
	}

	/**
	 * Perform asynchronous food set updating
	 * 
	 * @param foodSetInfo
	 * @param include
	 */
	void updateSet(final FoodSetInfo foodSetInfo, final boolean include)
	{
		new AsyncTask<Void, Void, Boolean>()
		{
			@Override
			protected void onPreExecute()
			{
				UIUtils.showTip(ActivityFoodSet.this, getString(R.string.foodset_tip_loading_started));
			}

			@Override
			protected Boolean doInBackground(Void... params)
			{
				try
				{
					FoodSetService foodSetService = new FoodSetService(Storage.webClient);
					List<Versioned<FoodItem>> foodSet = foodSetService.getFoodSet(foodSetInfo.getId());

					for (Versioned<FoodItem> food : foodSet)
					{
						food.setDeleted(!include);

						Versioned<FoodItem> temp = Storage.localFoodBase.findById(food.getId());
						if (temp != null)
						{
							food.setVersion(temp.getVersion() + 1);
						}
					}

					Storage.localFoodBase.save(foodSet);
					return true;
				}
				catch (Exception e)
				{
					e.printStackTrace();
					return false;
				}
			}

			@Override
			protected void onPostExecute(Boolean succeed)
			{
				String tip;

				if (succeed)
				{
					if (include)
					{
						tip = getString(R.string.foodset_tip_set_add_ok);
						includedFoodSets.add(foodSetInfo.getId());
					}
					else
					{
						tip = getString(R.string.foodset_tip_set_remove_ok);
						includedFoodSets.remove(foodSetInfo.getId());
					}

					for (int i = 0; i < data.size(); i++)
					{
						if (data.get(i).getId().equals(foodSetInfo.getId()))
						{
							listFoodSets.setItemChecked(i, include);
							break;
						}
					}

					syncablePreferences.setStringSet(Preference.FOOD_SETS, includedFoodSets);
				}
				else
				{
					if (include)
					{
						tip = getString(R.string.foodset_tip_set_add_fail);
					}
					else
					{
						tip = getString(R.string.foodset_tip_set_remove_fail);
					}
				}

				UIUtils.showTip(ActivityFoodSet.this, String.format(tip, foodSetInfo.getDescription()));
			}
		}.execute();
	}

	// @Override
	// public boolean onCreateOptionsMenu(Menu menu)
	// {
	// // Inflate the menu; this adds items to the action bar if it is present.
	// getMenuInflater().inflate(R.menu.activity_food_set, menu);
	// return true;
	// }

	// @Override
	// public boolean onOptionsItemSelected(MenuItem item)
	// {
	// // Handle action bar item clicks here. The action bar will
	// // automatically handle clicks on the Home/Up button, so long
	// // as you specify a parent activity in AndroidManifest.xml.
	// int id = item.getItemId();
	// if (id == R.id.action_settings)
	// {
	// return true;
	// }
	// return super.onOptionsItemSelected(item);
	// }
}
