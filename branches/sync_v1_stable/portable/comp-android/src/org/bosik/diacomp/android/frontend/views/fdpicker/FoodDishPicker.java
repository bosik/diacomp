package org.bosik.diacomp.android.frontend.views.fdpicker;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.android.v1.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;
import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Filter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

enum ItemType
{
	FOOD, DISH
}

class Item implements Comparable<Item>
{
	private final ItemType				type;
	private final NamedRelativeTagged	data;

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
	List<Item>	itemsAll;
	List<Item>	suggestions;
	private int	viewResourceId;

	public ItemAdapter(Context context, int viewResourceId, List<Item> items)
	{
		super(context, viewResourceId, items);

		this.itemsAll = new ArrayList<Item>();
		itemsAll.addAll(items);
		this.suggestions = new ArrayList<Item>();
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

			ImageView itemIcon = (ImageView) v.findViewById(R.id.itemIcon);

			int iconResId = FoodDishPicker.iconMap.get(item.getType());
			itemIcon.setImageResource(iconResId);
		}
		return v;
	}

	@Override
	public Filter getFilter()
	{
		return filter;
	}

	Filter	filter	= new Filter()
					{
						@Override
						public String convertResultToString(Object resultValue)
						{
							String str = ((Item) resultValue).getCaption();
							return str;
						}

						@Override
						protected FilterResults performFiltering(CharSequence constraint)
						{
							if (constraint != null)
							{
								List<Item> firstList = new ArrayList<Item>();
								List<Item> secondList = new ArrayList<Item>();

								String search = constraint.toString().toLowerCase();
								for (Item item : itemsAll)
								{
									String line = item.getCaption().toLowerCase();

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
								@SuppressWarnings("unchecked")
								List<Item> filteredList = (List<Item>) results.values;
								addAll(filteredList);
							}

							notifyDataSetChanged();
						}
					};

}

/**
 * Composite component: autocomplete box + mass input + submit button
 * 
 * @author Bosik
 * 
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

	FoodDishTextView							editName;
	EditText									editMass;
	Button										buttonSubmit;

	private OnSubmitListener					onSubmit;

	public static final Map<ItemType, Integer>	iconMap	= new HashMap<ItemType, Integer>();
	static
	{
		iconMap.clear();
		// FIXME: use separated resources
		iconMap.put(ItemType.FOOD, R.drawable.button_foodbase);
		iconMap.put(ItemType.DISH, R.drawable.button_dishbase);
	}

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
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_fooddishpicker, this);

		if (!isInEditMode())
		{
			editName = (FoodDishTextView) findViewById(R.id.fdPickerAutocomplete);
			editMass = (EditText) findViewById(R.id.fdPickerMass);
			buttonSubmit = (Button) findViewById(R.id.fdPickerSubmit);

			editName.setOnItemClickListener(new OnItemClickListener()
			{
				@Override
				public void onItemClick(AdapterView<?> arg0, View arg1, int position, long id)
				{
					editMass.requestFocus();
				}
			});
			editMass.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
			editMass.setOnEditorActionListener(new TextView.OnEditorActionListener()
			{
				// TODO: clarify if it works or not

				@Override
				public boolean onEditorAction(TextView v, int actionId, KeyEvent event)
				{
					// super onEditorAction(v, actionId, event);

					if (actionId == EditorInfo.IME_ACTION_SEARCH)
					{
						// editMass.requestFocus();
						Log.d("XXX", "It works!");
						return true;
					}

					if (actionId == EditorInfo.IME_ACTION_UNSPECIFIED)
					{
						Log.d("XXX", "Enter pressed");
						submit();
						return true;
					}

					return false;
				}
			});
			buttonSubmit.setOnClickListener(new OnClickListener()
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
		// preparing storages
		List<Versioned<FoodItem>> foodBase = Storage.localFoodBase.findAll(false);
		List<Versioned<DishItem>> dishBase = Storage.localDishBase.findAll(false);
		Map<String, Integer> tagInfo = Storage.tagService.getTags();

		List<Item> data = new ArrayList<Item>();

		for (Versioned<FoodItem> item : foodBase)
		{
			Integer tag = tagInfo.get(item.getId());
			item.getData().setTag(tag != null ? tag : 0);
			data.add(new Item(item.getData()));
		}

		for (Versioned<DishItem> item : dishBase)
		{
			Integer tag = tagInfo.get(item.getId());
			item.getData().setTag(tag != null ? tag : 0);
			data.add(new Item(item.getData()));
		}

		Collections.sort(data);

		setData(data);
	}

	public void setData(List<Item> data)
	{
		ItemAdapter adapter = new ItemAdapter(getContext(), R.layout.view_iconed_line, data);
		editName.setAdapter(adapter);
	}

	public void focusName()
	{
		editName.requestFocus();
	}

	public void focusMass()
	{
		editMass.requestFocus();
	}

	void submit()
	{
		String name = editName.getText().toString();
		if (name.trim().isEmpty())
		{
			// TODO: localize
			UIUtils.showTip((Activity) getContext(), "Enter food/dish name");
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
			UIUtils.showTip((Activity) getContext(), "Enter correct mass");
			editMass.requestFocus();
			return;
		}

		if (!FoodMassed.checkMass(mass))
		{
			// TODO: localize
			UIUtils.showTip((Activity) getContext(), "Enter correct mass");
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