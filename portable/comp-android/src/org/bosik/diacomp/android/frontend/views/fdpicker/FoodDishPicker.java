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

import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.Filter;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.dishbase.LocalDishBase;
import org.bosik.diacomp.android.backend.features.foodbase.LocalFoodBase;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

enum ItemType
{
	FOOD,
	DISH
}

class Item implements Comparable<Item>
{
	private final ItemType            type;
	private final NamedRelativeTagged data;

	public Item(FoodItem food)
	{
		this.type = ItemType.FOOD;
		this.data = food;
	}

	public Item(DishItem dish)
	{
		this.type = ItemType.DISH;
		this.data = dish;
	}

	public ItemType getType()
	{
		return type;
	}

	public String getCaption()
	{
		return data.getName();
	}

	public NamedRelativeTagged getData()
	{
		return data;
	}

	@Override
	public int compareTo(Item rhs)
	{
		if (getData().getTag() == rhs.getData().getTag())
		{
			return getData().getName().compareTo(rhs.getData().getName());
		}
		else
		{
			return rhs.getData().getTag() - getData().getTag();
		}
	}
}

class ItemAdapter extends ArrayAdapter<Item>
{
	private List<Item> itemsAll;
	private List<Item> suggestions;
	private int        viewResourceId;

	public ItemAdapter(Context context, int viewResourceId, List<Item> items)
	{
		super(context, viewResourceId, items);

		this.itemsAll = new ArrayList<>(items);
		this.suggestions = new ArrayList<>();
		this.viewResourceId = viewResourceId;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent)
	{
		View v = convertView;
		if (v == null)
		{
			LayoutInflater vi = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			v = vi.inflate(viewResourceId, null);
		}

		if (position < suggestions.size())
		{
			Item item = suggestions.get(position);

			TextView itemCaption = (TextView) v.findViewById(R.id.itemDescription);
			itemCaption.setText(item.getCaption());

			switch (item.getType())
			{
				// FIXME: use separated resources
				case FOOD:
				{
					itemCaption.setCompoundDrawablesWithIntrinsicBounds(R.drawable.button_foodbase, 0, 0, 0);
					break;
				}
				case DISH:
				{
					itemCaption.setCompoundDrawablesWithIntrinsicBounds(R.drawable.button_dishbase, 0, 0, 0);
					break;
				}
				default:
				{
					throw new IllegalArgumentException("Invalid item type: " + item.getType());
				}
			}
		}

		return v;
	}

	@Override
	public Filter getFilter()
	{
		return filter;
	}

	private Filter filter = new Filter()
	{
		@Override
		public String convertResultToString(Object resultValue)
		{
			return ((Item) resultValue).getCaption();
		}

		@Override
		protected FilterResults performFiltering(CharSequence constraint)
		{
			if (constraint != null)
			{
				List<Item> firstList = new ArrayList<>();
				List<Item> secondList = new ArrayList<>();

				String search = constraint.toString().toLowerCase(Locale.US);
				for (Item item : itemsAll)
				{
					String line = item.getCaption().toLowerCase(Locale.US);

					if (Utils.hasWordStartedWith(line, search))
					{
						firstList.add(item);
					}
					else if (line.contains(search))
					{
						secondList.add(item);
					}
				}

				suggestions.clear();
				suggestions.addAll(firstList);
				suggestions.addAll(secondList);

				FilterResults filterResults = new FilterResults();
				filterResults.values = suggestions;
				filterResults.count = suggestions.size();

				return filterResults;
			}
			else
			{
				return new FilterResults();
			}
		}

		@Override
		protected void publishResults(CharSequence constraint, FilterResults results)
		{
			clear();

			if (results != null && results.values != null)
			{
				@SuppressWarnings("unchecked") List<Item> filteredList = (List<Item>) results.values;
				addAll(filteredList);
			}

			notifyDataSetChanged();
		}
	};
}

/**
 * Composite component: autocomplete box + mass input + submit button
 */
public class FoodDishPicker extends LinearLayout
{
	// ===================================== CALLBACKS ======================================

	public interface OnSubmitListener
	{
		/**
		 * Called when user submits massed item by clicking submit button
		 *
		 * @param text
		 * @param mass
		 * @return Whether the data is successfully validated & accepted
		 */
		boolean onSubmit(String text, double mass);
	}

	// TODO
	// public interface OnErrorMassListener {}

	// ===================================== FIELDS ======================================

	private FoodDishTextView editName;
	private EditText         editMass;

	private OnSubmitListener onSubmit;

	// ===================================== METHODS ======================================

	public FoodDishPicker(Context context)
	{
		super(context);
		init(context);
	}

	public FoodDishPicker(Context context, AttributeSet attrs)
	{
		super(context, attrs);
		init(context);
	}

	public FoodDishPicker(Context context, AttributeSet attrs, int defStyle)
	{
		super(context, attrs, defStyle);
		init(context);
	}

	private void init(Context context)
	{
		LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_fooddishpicker, this);

		if (!isInEditMode())
		{
			editName = (FoodDishTextView) findViewById(R.id.fdPickerAutocomplete);
			editName.setOnItemClickListener(new OnItemClickListener()
			{
				@Override
				public void onItemClick(AdapterView<?> arg0, View arg1, int position, long id)
				{
					editMass.requestFocus();
				}
			});

			editMass = (EditText) findViewById(R.id.fdPickerMass);
			editMass.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
			editMass.setOnEditorActionListener(new TextView.OnEditorActionListener()
			{
				@Override
				public boolean onEditorAction(TextView v, int actionId, KeyEvent event)
				{
					if (actionId == R.id.actionAddFood || actionId == EditorInfo.IME_NULL)
					{
						submit();
						return true;
					}

					return false;
				}
			});

			findViewById(R.id.fdPickerSubmit).setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					submit();
				}
			});

			loadItemsList();
		}
	}

	public void setOnSubmitLister(OnSubmitListener l)
	{
		onSubmit = l;
	}

	private void loadItemsList()
	{
		// prepare sources

		FoodBaseService foodBase = LocalFoodBase.getInstance(getContext());
		DishBaseService dishBase = LocalDishBase.getInstance(getContext());

		// build lists

		List<Item> data = new ArrayList<>();

		for (Versioned<FoodItem> item : foodBase.findAll(false))
		{
			data.add(new Item(item.getData()));
		}

		for (Versioned<DishItem> item : dishBase.findAll(false))
		{
			data.add(new Item(item.getData()));
		}

		// sort

		Collections.sort(data);

		editName.setAdapter(new ItemAdapter(getContext(), R.layout.view_iconed_line, data));
	}

	public void focusName()
	{
		editName.requestFocus();
	}

	public void focusMass()
	{
		editMass.requestFocus();
	}

	private void submit()
	{
		String name = editName.getText().toString();
		if (name.trim().isEmpty())
		{
			UIUtils.showTip((Activity) getContext(), getContext().getString(R.string.fd_tip_empty));
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
			UIUtils.showTip((Activity) getContext(), getContext().getString(R.string.fd_tip_incorrect_mass));
			editMass.requestFocus();
			return;
		}

		if (!FoodMassed.checkMass(mass))
		{
			UIUtils.showTip((Activity) getContext(), getContext().getString(R.string.fd_tip_incorrect_mass));
			editMass.requestFocus();
			return;
		}

		if (onSubmit != null)
		{
			if (onSubmit.onSubmit(name, mass))
			{
				editMass.setText("");
				editName.setText("");
				editName.requestFocus();
			}
		}
	}
}
