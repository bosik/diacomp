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
package org.bosik.diacomp.android.frontend.views.fdpicker;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.dishbase.LocalDishBase;
import org.bosik.diacomp.android.backend.features.foodbase.LocalFoodBase;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.UIUtils.OnSubmit;
import org.bosik.diacomp.android.frontend.views.fdpicker.FoodDishPicker.OnSubmitListener;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import android.app.Activity;
import android.content.ContentResolver;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;

public class MealEditorView extends LinearLayout
{
	// callbacks
	public interface OnChangeListener
	{
		void onChange(final List<FoodMassed> items);
	}

	// components
	ListView			list;
	FoodDishPicker		fdPicker;

	// localization
	String				captionCarbs;
	String				captionDose;
	String				captionGramm;

	// listeners
	OnChangeListener	onChange;

	// data
	List<FoodMassed>	data	= new ArrayList<FoodMassed>();

	public MealEditorView(Context context)
	{
		super(context);
		init(context);
	}

	public MealEditorView(Context context, AttributeSet attrs)
	{
		super(context, attrs);
		init(context);
	}

	public MealEditorView(Context context, AttributeSet attrs, int defStyle)
	{
		super(context, attrs, defStyle);
		init(context);
	}

	private void init(final Context context)
	{
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_mealeditorview, this);

		if (!isInEditMode())
		{
			// string constants
			captionCarbs = context.getString(R.string.editor_meal_label_stat);
			captionDose = context.getString(R.string.common_unit_insulin);
			captionGramm = context.getString(R.string.common_unit_mass_gramm);

			// components
			list = (ListView) findViewById(R.id.mealEditorList);
			fdPicker = (FoodDishPicker) findViewById(R.id.mealEditorPicker);

			list.setOnItemClickListener(new OnItemClickListener()
			{
				@Override
				public void onItemClick(AdapterView<?> parent, View view, final int position, long id)
				{
					// TODO: i18n

					final String title = context.getString(R.string.editor_mass_title);
					final String message = data.get(position).getName() + ", " + captionGramm;
					final String defaultMass = Utils.formatDoubleShort(data.get(position).getMass());

					UIUtils.requestMass(context, title, message, defaultMass, new OnSubmit()
					{
						@Override
						public void onSubmit(Double mass)
						{
							if (mass == null)
							{
								data.remove(position);
							}
							else
							{
								data.get(position).setMass(mass);
							}

							if (onChange != null)
							{
								onChange.onChange(data);
							}

							showData();
						}

						@Override
						public void onCancel()
						{
							// Do nothing.
						}
					});
				}
			});
			fdPicker.setOnSubmitLister(new OnSubmitListener()
			{
				@Override
				public boolean onSubmit(String name, double mass)
				{
					ContentResolver resolver = getContext().getContentResolver();

					// try to search item in food base

					FoodBaseService foodBase = LocalFoodBase.getInstance(getContext());
					Versioned<FoodItem> food = foodBase.findOne(name);

					if (food != null)
					{
						data.add(new FoodMassed(food.getData(), mass));

						if (onChange != null)
						{
							onChange.onChange(data);
						}

						showData();
						return true;
					}

					// try to search item in dish base

					DishBaseService dishBase = LocalDishBase.getInstance(getContext());
					Versioned<DishItem> dish = dishBase.findOne(name);

					if (dish != null)
					{
						data.add(new FoodMassed(dish.getData(), mass));

						if (onChange != null)
						{
							onChange.onChange(data);
						}

						showData();
						return true;
					}

					UIUtils.showTip((Activity) context,
							String.format(context.getString(R.string.fd_tip_item_not_found), name));
					fdPicker.focusName();
					return false;
				}
			});
		}
	}

	void showData()
	{
		final String[] temp = new String[data.size()];
		for (int i = 0; i < data.size(); i++)
		{
			temp[i] = "stub";
		}

		ArrayAdapter<String> adapter = new ArrayAdapter<String>(getContext(), R.layout.view_meal_item,
				android.R.id.text1, temp)
		{
			@Override
			public View getView(int position, View convertView, ViewGroup parent)
			{
				View view = super.getView(position, convertView, parent);
				TextView text1 = (TextView) view.findViewById(android.R.id.text1);
				TextView text2 = (TextView) view.findViewById(android.R.id.text2);

				text1.setText(data.get(position).getName());
				text2.setText(Utils.formatDoubleShort(data.get(position).getMass()) + " " + captionGramm);
				return view;
			}
		};
		list.setAdapter(adapter);
	}

	public List<FoodMassed> getData()
	{
		return data;
	}

	public void setData(List<FoodMassed> data)
	{
		if (data != null)
		{
			this.data = data;
			showData();
		}
		else
		{
			throw new IllegalArgumentException("Data is null");
		}
	}

	public void setOnChangeListener(OnChangeListener l)
	{
		onChange = l;
	}
}
