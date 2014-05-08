package org.bosik.diacomp.android.frontend.views.fdpicker;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.android.R;
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
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.SimpleAdapter;
import android.widget.TextView;

public class FoodDishPicker extends LinearLayout
{
	// ===================================== CLASSES ======================================

	public enum ItemType
	{
		FOOD, DISH
	}

	public static class Item implements Comparable<Item>
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

	private static final Map<ItemType, String>	iconMap	= new HashMap<ItemType, String>();
	{
		iconMap.clear();
		// FIXME: use separated resources
		iconMap.put(ItemType.FOOD, Integer.toString(R.drawable.button_foodbase));
		iconMap.put(ItemType.DISH, Integer.toString(R.drawable.button_dishbase));
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
					@SuppressWarnings("unchecked")
					HashMap<String, String> hm = (HashMap<String, String>) arg0.getAdapter().getItem(position);
					String caption = hm.get(FoodDishTextView.FIELD_CAPTION);
					System.out.println("Clicked item: " + caption);

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
		List<HashMap<String, String>> aList = new ArrayList<HashMap<String, String>>();

		for (Item item : data)
		{
			HashMap<String, String> hm = new HashMap<String, String>();

			hm.put(FoodDishTextView.FIELD_ICON, iconMap.get(item.getType()));
			hm.put(FoodDishTextView.FIELD_CAPTION, item.getCaption());

			aList.add(hm);
		}

		// Keys used in Hashmap
		String[] from = { FoodDishTextView.FIELD_ICON, FoodDishTextView.FIELD_CAPTION };

		// Ids of views in layout
		// TODO: rename R.id.*
		int[] to = { R.id.itemIcon, R.id.itemDescription };

		editName.setAdapter(new SimpleAdapter(getContext(), aList, R.layout.view_iconed_line, from, to));
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

		if (FoodDishPicker.this.onSubmit != null)
		{
			if (FoodDishPicker.this.onSubmit.onSubmit(name, mass))
			{
				editMass.setText("");
				editName.setText("");
				editName.requestFocus();
			}
		}
	}
}
