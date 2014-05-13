package org.bosik.diacomp.android.frontend.views.fdpicker;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.UIUtils.OnSubmit;
import org.bosik.diacomp.android.frontend.views.fdpicker.FoodDishPicker.OnSubmitListener;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;
import android.app.Activity;
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
			captionCarbs = context.getString(R.string.editor_meal_label_carbs);
			captionDose = context.getString(R.string.editor_meal_label_dose);
			captionGramm = context.getString(R.string.common_gramm);

			// components
			list = (ListView) findViewById(R.id.mealEditorList);
			fdPicker = (FoodDishPicker) findViewById(R.id.mealEditorPicker);

			list.setOnItemClickListener(new OnItemClickListener()
			{
				@Override
				public void onItemClick(AdapterView<?> parent, View view, final int position, long id)
				{
					// TODO: localize

					final String title = "Change mass";
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
					// try to search item in food base

					Versioned<FoodItem> foodItem = Storage.localFoodBase.findOne(name);

					if (foodItem != null)
					{
						FoodItem food = foodItem.getData();

						FoodMassed item = new FoodMassed();
						item.setName(food.getName());
						item.setRelProts(food.getRelProts());
						item.setRelFats(food.getRelFats());
						item.setRelCarbs(food.getRelCarbs());
						item.setRelValue(food.getRelValue());
						item.setMass(mass);

						data.add(item);
						if (onChange != null)
						{
							onChange.onChange(data);
						}

						showData();
						return true;
					}

					// try to search item in dish base

					List<Versioned<DishItem>> listDish = Storage.localDishBase.findAny(name);

					if (!listDish.isEmpty())
					{
						DishItem dish = listDish.get(0).getData();

						FoodMassed item = new FoodMassed();
						item.setName(dish.getName());
						item.setRelProts(dish.getRelProts());
						item.setRelFats(dish.getRelFats());
						item.setRelCarbs(dish.getRelCarbs());
						item.setRelValue(dish.getRelValue());
						item.setMass(mass);

						data.add(item);
						if (onChange != null)
						{
							onChange.onChange(data);
						}

						showData();
						return true;
					}

					UIUtils.showTip((Activity) context, "Item not found: " + name);
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

		ArrayAdapter<String> adapter = new ArrayAdapter<String>(getContext(), android.R.layout.simple_list_item_2,
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
			throw new NullPointerException("Data can't be null");
		}
	}

	public void setOnChangeListener(OnChangeListener l)
	{
		onChange = l;
	}
}
