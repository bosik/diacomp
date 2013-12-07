package org.bosik.compensation.face.activities;

import java.util.ArrayList;
import java.util.List;
import org.bosik.compensation.bo.IRelative;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.face.R;
import org.bosik.compensation.persistence.Storage;
import android.app.Activity;
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

	// Widgets
	private EditText		editFoodSearch;
	private ListView		listFood;

	// Data
	private List<IRelative>	data;

	// ===========================================================================

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_foodbase);

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
		new AsyncTask<String, Void, List<IRelative>>()
		{
			@Override
			protected List<IRelative> doInBackground(String... params)
			{
				return request(params[0]);
			}

			@Override
			protected void onPostExecute(List<IRelative> result)
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

	private List<IRelative> request(String filter)
	{
		List<FoodItem> temp;
		if (filter.trim().isEmpty())
		{
			temp = Storage.localFoodBase.findAll();
		}
		else
		{
			temp = Storage.localFoodBase.findAny(filter);
		}

		List<IRelative> result = new ArrayList<IRelative>();
		result.addAll(temp);
		return result;
	}

	private void showBase(final List<IRelative> foodBase)
	{
		data = foodBase;

		String[] str = new String[foodBase.size()];
		for (int i = 0; i < foodBase.size(); i++)
		{
			str[i] = foodBase.get(i).getName();
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

				text1.setText(foodBase.get(position).getName());
				text2.setText(getInfo(foodBase.get(position)));
				return view;
			}
		};

		listFood.setAdapter(adapter);
	}

	private String getInfo(IRelative foodItem)
	{
		String fmt = getString(R.string.foodbase_subinfo, foodItem.getRelProts(), foodItem.getRelFats(),
				foodItem.getRelCarbs(), foodItem.getRelValue());
		return fmt;
	}
}
