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

import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.FragmentActivity;
import android.util.SparseBooleanArray;
import android.view.ActionMode;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import com.jjoe64.graphview.series.DataPoint;
import com.jjoe64.graphview.series.LineGraphSeries;
import com.jjoe64.graphview.series.Series;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.analyze.RateServiceInternal;
import org.bosik.diacomp.android.backend.features.analyze.RateServiceManual;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.frontend.fragments.FragmentMassUnitDialog;
import org.bosik.diacomp.android.frontend.fragments.chart.Chart;
import org.bosik.diacomp.android.frontend.fragments.chart.ProgressBundle;
import org.bosik.diacomp.core.entities.business.TimedRate;
import org.bosik.diacomp.core.entities.business.Units;
import org.bosik.diacomp.core.services.analyze.RateService;
import org.bosik.diacomp.core.services.analyze.entities.Rate;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.CodedUtils;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.Versioned;
import org.json.JSONException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class ActivityRates extends FragmentActivity implements DialogInterface.OnClickListener
{
	// Constants
	private static final int    DIALOG_RATE_CREATE = 11;
	private static final int    DIALOG_RATE_MODIFY = 12;
	private static final String KEY_RATES          = "org.bosik.diacomp.android.frontend.activities.RATES";
	private static final String KEY_HISTORY        = "org.bosik.diacomp.android.frontend.activities.HISTORY";
	private static final String KEY_HISTORY_INDEX  = "org.bosik.diacomp.android.frontend.activities.HISTORY_INDEX";

	// components
	private Chart       chart;
	private ListView    list;
	private BaseAdapter adapter;

	// data
	private ArrayList<Versioned<TimedRate>>       rates; // must be serializable
	private ArrayList<List<Versioned<TimedRate>>> history; // must be serializable
	private int                                   historyIndex;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_rates);

		// THINK: double loading occurs here

		final RateService ratesService = new RateServiceManual(this);

		if (savedInstanceState != null && savedInstanceState.containsKey(KEY_RATES))
		{
			rates = (ArrayList<Versioned<TimedRate>>) savedInstanceState.getSerializable(KEY_RATES);
		}
		else
		{
			List<Versioned<TimedRate>> versioned = Versioned.wrap(RateServiceManual.loadRates(this));
			Versioned.regenerateIds(versioned);
			rates = new ArrayList<>(versioned);
		}

		if (savedInstanceState != null && savedInstanceState.containsKey(KEY_HISTORY))
		{
			history = (ArrayList<List<Versioned<TimedRate>>>) savedInstanceState.getSerializable(KEY_HISTORY);
		}
		else
		{
			history = new ArrayList<>();
			history.add(new ArrayList<>(rates));
		}

		if (savedInstanceState != null && savedInstanceState.containsKey(KEY_HISTORY_INDEX))
		{
			historyIndex = savedInstanceState.getInt(KEY_HISTORY_INDEX);
		}
		else
		{
			historyIndex = 0;
		}

		list = findViewById(R.id.listRates);
		list.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE_MODAL);
		list.setOnItemClickListener(new AdapterView.OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long itemIndex)
			{
				showRateEditor(rates.get(position), false);
			}
		});
		list.setMultiChoiceModeListener(new AbsListView.MultiChoiceModeListener()
		{
			@Override
			public void onItemCheckedStateChanged(ActionMode actionMode, int i, long l, boolean b)
			{
				int selectedCount = list.getCheckedItemCount();
				setSubtitle(actionMode, selectedCount);
			}

			@Override
			public boolean onCreateActionMode(ActionMode actionMode, Menu menu)
			{
				MenuInflater inflater = actionMode.getMenuInflater();
				inflater.inflate(R.menu.actions_rates_context, menu);
				return true;
			}

			@Override
			public boolean onPrepareActionMode(ActionMode actionMode, Menu menu)
			{
				return false;
			}

			@Override
			public boolean onActionItemClicked(ActionMode actionMode, MenuItem menuItem)
			{
				SparseBooleanArray checkList = list.getCheckedItemPositions();
				Set<String> ids = new HashSet<>();
				for (int i = 0; i < checkList.size(); i++)
				{
					if (checkList.valueAt(i))
					{
						int index = checkList.keyAt(i);
						if (index >= 0 && index < rates.size())
						{
							ids.add(rates.get(index).getId());
						}
					}
				}

				if (!ids.isEmpty())
				{
					int count = 0;
					for (Iterator<Versioned<TimedRate>> i = rates.iterator(); i.hasNext(); )
					{
						if (ids.contains(i.next().getId()))
						{
							i.remove();
							count++;
						}
					}

					save(rates);
					saveStateToHistory();
					adapter.notifyDataSetChanged();

					String text = String.format(getString(R.string.base_tip_items_removed), count); // FIXME
					Toast.makeText(list.getContext(), text, Toast.LENGTH_LONG).show();
				}

				return true;
			}

			@Override
			public void onDestroyActionMode(ActionMode actionMode)
			{
			}

			private void setSubtitle(ActionMode mode, int selectedCount)
			{
				mode.setSubtitle(selectedCount == 0 ? null : String.valueOf(selectedCount));
			}
		});

		final PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));

		adapter = new BaseAdapter()
		{
			@Override
			public int getCount()
			{
				return rates.size();
			}

			private Versioned<TimedRate> getItemCasted(int position)
			{
				if (position >= 0 && position < rates.size())
				{
					return rates.get(position);
				}
				else
				{
					return null;
				}
			}

			@Override
			public Object getItem(int position)
			{
				return getItemCasted(position);
			}

			@Override
			public boolean hasStableIds()
			{
				return true;
			}

			@Override
			public long getItemId(int position)
			{
				Versioned<TimedRate> item = getItemCasted(position);
				if (item != null)
				{
					return item.getData().getTime();
				}
				else
				{
					return position;
				}
			}

			@Override
			public View getView(int position, View view, ViewGroup parent)
			{
				Versioned<TimedRate> rate = getItemCasted(position);

				if (rate != null)
				{
					if (view == null)
					{
						view = getLayoutInflater().inflate(R.layout.view_rate, null);
					}

					String preferenceValue = preferences.getStringValue(PreferenceID.RATES_MASS_UNITS);
					Units.Mass unit = CodedUtils.parse(Units.Mass.class, preferenceValue, Utils.DEFAULT_MASS_UNIT);

					((TextView) view.findViewById(R.id.ratesItemTime)).setText(Utils.formatTimeMin(rate.getData().getTime()));

					((TextView) view.findViewById(R.id.ratesItemK)).setText(formatK(rate.getData(), unit));
					((TextView) view.findViewById(R.id.ratesItemKUnit)).setText(formatKUnit(unit));

					((TextView) view.findViewById(R.id.ratesItemQ)).setText(formatQ(rate.getData()));
					((TextView) view.findViewById(R.id.ratesItemQUnit)).setText(formatQUnit());

					((TextView) view.findViewById(R.id.ratesItemX)).setText(formatX(rate.getData(), unit));
					((TextView) view.findViewById(R.id.ratesItemXUnit)).setText(formatXUnit(unit));
				}

				view.setBackgroundDrawable(getResources().getDrawable(R.drawable.background_base_item));
				return view;
			}

			@Override
			public void notifyDataSetChanged()
			{
				super.notifyDataSetChanged();
				invalidateOptionsMenu();
				if (chart != null)
				{
					chart.refresh();
				}
			}
		};

		list.setAdapter(adapter);

		chart = (Chart) getSupportFragmentManager().findFragmentById(R.id.ratesChart);

		if (chart == null)
		{
			chart = new Chart();
			getSupportFragmentManager().beginTransaction().add(R.id.ratesChart, chart).commit();
		}

		chart.setChartType(Chart.ChartType.DAILY);
		updateChartTitle();
		chart.setDescription(getString(R.string.common_rate_x_description) + ". " + getString(R.string.charts_type_daily) + ".");
		chart.setDataLoader(new ProgressBundle.DataLoader()
		{
			@Override
			public Collection<Series<?>> load(Context context)
			{
				ratesService.update();
				Units.Mass unitOfMass = getMassUnit();
				List<DataPoint> dataAvg = new ArrayList<>();
				for (int time = 0; time < Utils.MinPerDay; time += 5)
				{
					Rate rate = ratesService.getRate(time);
					if (rate != null)
					{
						double valueOriginal = rate.getK() / rate.getQ();
						double valueConverted = Units.Mass.convert(valueOriginal, Units.Mass.G, unitOfMass);
						dataAvg.add(new DataPoint((double) time / Utils.MinPerHour, valueConverted));
					}
				}

				LineGraphSeries<DataPoint> seriesAvg = new LineGraphSeries<>(dataAvg.toArray(new DataPoint[dataAvg.size()]));
				seriesAvg.setColor(getResources().getColor(R.color.charts_x));

				return Collections.<Series<?>>singletonList(seriesAvg);
			}
		});
	}

	@Override
	protected void onSaveInstanceState(Bundle outState)
	{
		super.onSaveInstanceState(outState);

		if (outState != null)
		{
			outState.putSerializable(KEY_RATES, rates);
			outState.putSerializable(KEY_HISTORY, history);
			outState.putInt(KEY_HISTORY_INDEX, historyIndex);
		}
	}

	private void updateChartTitle()
	{
		String unitName = convertUnitToTitle(getMassUnit());
		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_rate_x), getString(R.string.common_unit_insulin), unitName));
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu)
	{
		menu.findItem(R.id.itemRatesUndo).setEnabled(historyIndex > 0);
		menu.findItem(R.id.itemRatesRedo).setEnabled(historyIndex < history.size() - 1);
		menu.findItem(R.id.itemRatesClear).setEnabled(!rates.isEmpty());
		return true;
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		super.onCreateOptionsMenu(menu);
		getMenuInflater().inflate(R.menu.actions_rates, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		switch (item.getItemId())
		{
			case R.id.itemRatesAdd:
			{
				showRateEditor(null, true);
				return true;
			}

			case R.id.itemRatesUndo:
			{
				historyUndo();

				save(rates);
				adapter.notifyDataSetChanged();
				return true;
			}

			case R.id.itemRatesRedo:
			{
				historyRedo();

				save(rates);
				adapter.notifyDataSetChanged();
				return true;
			}

			case R.id.itemRatesReplaceWithAuto:
			{
				RateService service = RateServiceInternal.getInstanceAuto(this);

				rates.clear();
				for (int time = 0; time < Utils.MinPerDay; time += 2 * Utils.MinPerHour)
				{
					Rate rate = service.getRate(time);
					if (rate != null)
					{
						Versioned<TimedRate> versioned = new Versioned<>(new TimedRate(time, rate));
						versioned.setId(HashUtils.generateGuid());
						rates.add(versioned);
					}
				}

				save(rates);
				saveStateToHistory();
				adapter.notifyDataSetChanged();
				return true;
			}

			case R.id.itemRatesUnitMass:
			{
				FragmentMassUnitDialog newFragment = new FragmentMassUnitDialog();
				Bundle args = new Bundle();
				args.putString(FragmentMassUnitDialog.KEY_UNIT_OF_MASS, getMassUnit().getCode());
				newFragment.setArguments(args);
				newFragment.show(getFragmentManager(), "unitMassPicker");
				return true;
			}

			case R.id.itemRatesClear:
			{
				if (!rates.isEmpty())
				{
					rates.clear();
					save(rates);
					saveStateToHistory();
					adapter.notifyDataSetChanged();
				}
				return true;
			}

			default:
			{
				return false;// super.onOptionsItemSelected(item);
			}
		}
	}

	private Units.Mass getMassUnit()
	{
		final PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));
		String value = preferences.getStringValue(PreferenceID.RATES_MASS_UNITS);
		return CodedUtils.parse(Units.Mass.class, value, Utils.DEFAULT_MASS_UNIT);
	}

	private void setMassUnit(Units.Mass unit)
	{
		final PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));
		preferences.setStringValue(PreferenceID.RATES_MASS_UNITS, unit.getCode());
	}

	@Override
	public void onClick(DialogInterface dialog, int which)
	{
		String selectedLabel = getResources().getStringArray(R.array.unit_mass_options)[which];
		Units.Mass unit = convertTitleToUnit(selectedLabel);
		setMassUnit(unit);
		adapter.notifyDataSetChanged();
		updateChartTitle();
	}

	private void saveStateToHistory()
	{
		history.add(new ArrayList<>(rates));
		historyIndex = history.size() - 1;
	}

	private void historyUndo()
	{
		if (historyIndex > 0)
		{
			historyIndex--;
			rates = new ArrayList<>(history.get(historyIndex));
		}
	}

	private void historyRedo()
	{
		if (historyIndex < history.size() - 1)
		{
			historyIndex++;
			rates = new ArrayList<>(history.get(historyIndex));
		}
	}

	private void showRateEditor(Versioned<TimedRate> entity, boolean createMode)
	{
		if (createMode)
		{
			TimedRate rec = new TimedRate();
			rec.setTime(0); // FIXME
			entity = new Versioned<>(rec);
		}

		Intent intent = new Intent(this, ActivityEditorRate.class);
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
		intent.putExtra(ActivityEditor.FIELD_CREATE_MODE, createMode);
		intent.putExtra(ActivityEditorRate.KEY_UNIT_OF_MASS, getMassUnit().getCode());

		startActivityForResult(intent, createMode ? DIALOG_RATE_CREATE : DIALOG_RATE_MODIFY);
	}

	private String formatK(TimedRate timedRate, Units.Mass unitOfMass)
	{
		return "K: " + Utils.formatK(timedRate.getK(), unitOfMass);
	}

	private String formatKUnit(Units.Mass unitOfMass)
	{
		String unitBS = getString(R.string.common_unit_bs_mmoll);
		String unitMass = convertUnitToTitle(unitOfMass);
		return unitBS + "/" + unitMass;
	}

	private String formatQ(TimedRate timedRate)
	{
		return "Q: " + Utils.formatQ(timedRate.getQ());
	}

	private String formatQUnit()
	{
		String unitBS = getString(R.string.common_unit_bs_mmoll);
		String unitDosage = getString(R.string.common_unit_insulin);
		return unitBS + "/" + unitDosage;
	}

	private String formatX(TimedRate timedRate, Units.Mass unitOfMass)
	{
		return "X: " + Utils.formatX(timedRate.getK() / timedRate.getQ(), unitOfMass);
	}

	private String formatXUnit(Units.Mass unitOfMass)
	{
		String unitDosage = getString(R.string.common_unit_insulin);
		String unitMass = convertUnitToTitle(unitOfMass);
		return unitDosage + "/" + unitMass;
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent intent)
	{
		super.onActivityResult(requestCode, resultCode, intent);

		switch (requestCode)
		{
			case DIALOG_RATE_CREATE:
			{
				if (resultCode == Activity.RESULT_OK)
				{
					rates.add((Versioned<TimedRate>) intent.getExtras().getSerializable(ActivityEditor.FIELD_ENTITY));

					sortByTime(rates);
					save(rates);
					saveStateToHistory();
					adapter.notifyDataSetChanged();
				}
				break;
			}

			case DIALOG_RATE_MODIFY:
			{
				if (resultCode == Activity.RESULT_OK)
				{
					Versioned<TimedRate> rec = (Versioned<TimedRate>) intent.getExtras().getSerializable(ActivityEditor.FIELD_ENTITY);
					rates.remove(rec);
					rates.add(rec);

					sortByTime(rates);
					save(rates);
					saveStateToHistory();
					adapter.notifyDataSetChanged();
				}
				break;
			}
		}
	}

	private void save(List<Versioned<TimedRate>> versioned)
	{
		try
		{
			PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));
			String json = TimedRate.writeList(Versioned.unwrap(versioned));
			preferences.setStringValue(PreferenceID.RATES_DATA, json);
		}
		catch (JSONException e)
		{
			throw new RuntimeException(e);
		}
	}

	private void sortByTime(List<Versioned<TimedRate>> rates)
	{
		Collections.sort(rates, new Comparator<Versioned<TimedRate>>()
		{
			@Override
			public int compare(Versioned<TimedRate> lhs, Versioned<TimedRate> rhs)
			{
				return lhs.getData().getTime() - rhs.getData().getTime();
			}
		});
	}

	private String convertUnitToTitle(Units.Mass unit)
	{
		switch (unit)
		{
			case G:
			{
				return getString(R.string.common_unit_mass_gramm);
			}

			case BU:
			{
				return getString(R.string.common_unit_mass_bu);
			}

			default:
			{
				throw new IllegalArgumentException("Unsupported unit of mass: " + unit);
			}
		}
	}

	private Units.Mass convertTitleToUnit(String title)
	{
		if (getString(R.string.common_unit_mass_bu).equals(title))
		{
			return Units.Mass.BU;
		}
		else if (getString(R.string.common_unit_mass_gramm).equals(title))
		{
			return Units.Mass.G;
		}
		else
		{
			throw new IllegalArgumentException("Unknown unit of mass: " + title);
		}
	}
}