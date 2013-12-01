package org.bosik.compensation.face.activities;

import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.face.R;
import org.bosik.compensation.persistence.Storage;
import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.widget.ArrayAdapter;
import android.widget.ListView;

public class ActivityFoodbase extends Activity
{
	private static final String	TAG	= ActivityFoodbase.class.getSimpleName();

	// Widgets
	private ListView			list;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_foodbase);

		// Widgets binding
		list = (ListView) findViewById(R.id.listFood);

		// Show data
		List<FoodItem> base = Storage.localFoodBase.findAll();
		showBase(base);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.diary_menu, menu);
		return true;
	}

	private void showBase(List<FoodItem> base)
	{
		String[] str = new String[base.size()];
		for (int i = 0; i < base.size(); i++)
		{
			str[i] = base.get(i).getName();
		}

		Log.e(TAG, "Food base total items: " + str.length);

		setTitle(String.format("%s (%d", getString(R.string.title_activity_foodbase), str.length));

		ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, str);
		list.setAdapter(adapter);
	}
}
