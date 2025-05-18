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
import android.text.SpannableString;
import android.text.style.BackgroundColorSpan;
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
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.Named;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

class ItemAdapter extends ArrayAdapter<Versioned<? extends Named>>
{
	private final List<Versioned<? extends Named>> itemsAll;
	private final List<Versioned<? extends Named>> suggestions;
	private final int                              viewResourceId;
	private       List<String>                     tokens;

	public ItemAdapter(Context context, int viewResourceId, List<Versioned<? extends Named>> items)
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
			Versioned<? extends Named> item = suggestions.get(position);

			TextView itemCaption = v.findViewById(R.id.itemDescription);
			itemCaption.setText(highlightOccurrences(
					item.getData().getName(),
					tokens,
					getContext().getResources().getColor(R.color.search_highlight, null)
			));

			// FIXME: use separated resources (not button's)
			if (item.getData() instanceof FoodItem)
			{
				itemCaption.setCompoundDrawablesWithIntrinsicBounds(R.drawable.button_foodbase, 0, 0, 0);
			}
			else if (item.getData() instanceof DishItem)
			{
				itemCaption.setCompoundDrawablesWithIntrinsicBounds(R.drawable.button_dishbase, 0, 0, 0);
			}
			else
			{
				throw new IllegalArgumentException("Invalid item type: " + item.getClass().getName());
			}
		}

		return v;
	}

	private static SpannableString highlightOccurrences(String text, List<String> tokens, int color)
	{
		final SpannableString s = new SpannableString(text);
		text = text.toLowerCase(Locale.US);

		if (tokens != null)
		{
			for (String t : tokens)
			{
				if (t.length() > 1)
				{
					final int start = text.indexOf(t);
					if (start >= 0)
					{
						s.setSpan(new BackgroundColorSpan(color), start, start + t.length(), 0);
					}
				}
			}
		}

		return s;
	}

	@Override
	public Filter getFilter()
	{
		return filter;
	}

	private final Filter filter = new Filter()
	{
		@Override
		public String convertResultToString(Object resultValue)
		{
			return ((Named) resultValue).getName();
		}

		@Override
		protected FilterResults performFiltering(CharSequence constraint)
		{
			if (constraint != null)
			{
				tokens = Utils.parseTokens(constraint.toString());
				suggestions.clear();

				for (Versioned<? extends Named> item : itemsAll)
				{
					if (Utils.matchesTokens(item.getData().getName(), tokens))
					{
						suggestions.add(item);
					}
				}

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
				@SuppressWarnings("unchecked") List<Versioned<? extends Named>> filteredList = (List<Versioned<? extends Named>>) results.values;
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
			editName = findViewById(R.id.fdPickerAutocomplete);
			editName.setOnItemClickListener(new OnItemClickListener()
			{
				@Override
				public void onItemClick(AdapterView<?> arg0, View arg1, int position, long id)
				{
					editMass.requestFocus();
				}
			});

			editName.setMaxLines(Integer.MAX_VALUE);
			editName.setHorizontallyScrolling(false);

			editMass = findViewById(R.id.fdPickerMass);
			editMass.setOnEditorActionListener(new TextView.OnEditorActionListener()
			{
				@Override
				public boolean onEditorAction(TextView v, int actionId, KeyEvent event)
				{
					if (actionId == EditorInfo.IME_ACTION_DONE)
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
		}
	}

	public void setOnSubmitLister(OnSubmitListener l)
	{
		onSubmit = l;
	}

	public void setSuggestionsData(List<Versioned<? extends Named>> data)
	{
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
