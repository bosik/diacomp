package org.bosik.diacomp.face.activities;

import java.util.ArrayList;
import java.util.List;
import org.bosik.compensation.bo.RelativeTagged;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.diacomp.face.R;
import org.bosik.diacomp.persistence.Storage;
import org.bosik.diacomp.services.Sorter;
import org.bosik.compensation.persistence.common.Versioned;
import android.app.Activity;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;

public class ActivityFoodbase extends Activity
{
	// private static final String TAG = ActivityFoodbase.class.getSimpleName();

	public static final String				FIELD_GUID	= "bosik.pack.guid";

	// Widgets
	private EditText						editFoodSearch;
	private ListView						listFood;

	// Data
	private List<Versioned<RelativeTagged>>	data;
	private static final Sorter<FoodItem>	sorter		= new Sorter<FoodItem>();

	// ===========================================================================

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.picker_foodbase);

		// Widgets binding
		editFoodSearch = (EditText) findViewById(R.id.editFoodSearch);
		editFoodSearch.addTextChangedListener(new TextWatcher()
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
				runSearch(s.toString());
			}
		});
		listFood = (ListView) findViewById(R.id.listFood);

		// Show data
		runSearch("");
	}

	private void runSearch(String key)
	{
		new AsyncTask<String, Void, List<Versioned<RelativeTagged>>>()
		{
			@Override
			protected List<Versioned<RelativeTagged>> doInBackground(String... params)
			{
				return request(params[0]);
			}

			@Override
			protected void onPostExecute(List<Versioned<RelativeTagged>> result)
			{
				showBase(result);
			}
		}.execute(key);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.diary_menu, menu);
		return true;
	}

	private List<Versioned<RelativeTagged>> request(String filter)
	{
		List<Versioned<FoodItem>> temp;
		if (filter.trim().isEmpty())
		{
			temp = Storage.localFoodBase.findAll();
		}
		else
		{
			temp = Storage.localFoodBase.findAny(filter);
			sorter.sort(temp, Sorter.Sort.RELEVANT);
		}

		// TODO: check the performance
		List<Versioned<RelativeTagged>> result = new ArrayList<Versioned<RelativeTagged>>();
		for (Versioned<FoodItem> item : temp)
		{
			result.add(new Versioned<RelativeTagged>(item.getData()));
		}

		return result;
	}

	private void showBase(final List<Versioned<RelativeTagged>> foodBase)
	{
		data = foodBase;

		String[] str = new String[foodBase.size()];
		for (int i = 0; i < foodBase.size(); i++)
		{
			str[i] = foodBase.get(i).getData().getName();
		}

		setTitle(String.format("%s (%d)", getString(R.string.foodbase_title), foodBase.size()));

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

		listFood.setAdapter(adapter);
		listFood.setOnItemClickListener(new OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id)
			{
				returnResult(data.get(position).getId());
			}
		});
	}

	private String getInfo(RelativeTagged item)
	{
		String fmt = getString(R.string.foodbase_subinfo, item.getRelProts(), item.getRelFats(), item.getRelCarbs(),
				item.getRelValue());
		// fmt = fmt.replaceAll(" / ", "\t\t");
		// fmt = fmt + "\t\tTAG=" + item.getTag();
		return fmt;
	}

	private void returnResult(String guid)
	{
		Intent intent = getIntent();
		intent.putExtra(FIELD_GUID, guid);
		setResult(RESULT_OK, intent);
		finish();
	}
}
