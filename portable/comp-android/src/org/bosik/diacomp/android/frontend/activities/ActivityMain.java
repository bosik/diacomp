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

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.AccountUtils;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.backend.features.analyze.BackgroundService;
import org.bosik.diacomp.android.backend.features.notifications.NotificationService;
import org.bosik.diacomp.android.backend.features.preferences.device.DevicePreferences;
import org.bosik.diacomp.android.frontend.fragments.FragmentTabBase;
import org.bosik.diacomp.android.frontend.fragments.FragmentTabCharts;
import org.bosik.diacomp.android.frontend.fragments.FragmentTabDiary;
import org.bosik.diacomp.android.utils.ErrorHandler;
import android.accounts.Account;
import android.app.Activity;
import android.content.ContentResolver;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

interface Page
{
	String getTitle();

	Fragment getContent();
}

public class ActivityMain extends FragmentActivity implements OnSharedPreferenceChangeListener
{
	/* =========================== CONSTANTS ================================ */

	static final String			TAG			= ActivityMain.class.getSimpleName();
	// private static final int RESULT_SPEECH_TO_TEXT = 620;
	private static final int	CODE_LOGIN	= 0;

	/* =========================== FIELDS ================================ */

	ViewPager					mViewPager;
	private Menu				cachedMenu;
	private SharedPreferences	preferences;

	/* =========================== METHODS ================================ */

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		try
		{
			setContentView(R.layout.activity_main);

			// Backend

			PreferenceManager.setDefaultValues(this, R.xml.preferences, false);
			preferences = PreferenceManager.getDefaultSharedPreferences(this);
			Storage.init(this);

			// Account sync
			Account account = AccountUtils.getAccount(this);

			if (account != null)
			{
				long SYNC_INTERVAL = 120; // sec
				ContentResolver.setIsSyncable(account, DiaryContentProvider.AUTHORITY, 1);
				ContentResolver.setSyncAutomatically(account, DiaryContentProvider.AUTHORITY, true);
				ContentResolver.addPeriodicSync(account, DiaryContentProvider.AUTHORITY, Bundle.EMPTY, SYNC_INTERVAL);
			}
			else
			{
				Log.w(TAG, "No account found");
			}

			// Frontend

			final List<Page> pages = new ArrayList<Page>();
			pages.add(new Page()
			{
				@Override
				public String getTitle()
				{
					return ActivityMain.this.getString(R.string.main_option_diary);
				}

				@Override
				public Fragment getContent()
				{
					return new FragmentTabDiary();
				}
			});
			pages.add(new Page()
			{
				@Override
				public String getTitle()
				{
					return ActivityMain.this.getString(R.string.main_option_bases);
				}

				@Override
				public Fragment getContent()
				{
					return new FragmentTabBase();
				}
			});
			pages.add(new Page()
			{
				@Override
				public String getTitle()
				{
					return ActivityMain.this.getString(R.string.main_option_charts);
				}

				@Override
				public Fragment getContent()
				{
					return new FragmentTabCharts();
				}
			});

			FragmentStatePagerAdapter mDemoCollectionPagerAdapter = new FragmentStatePagerAdapter(
					getSupportFragmentManager())
			{
				@Override
				public int getCount()
				{
					return pages.size();
				}

				@Override
				public Fragment getItem(int position)
				{
					return pages.get(position).getContent();
				}

				@Override
				public CharSequence getPageTitle(int position)
				{
					return pages.get(position).getTitle();
				}
			};
			mViewPager = (ViewPager) findViewById(R.id.pager);
			mViewPager.setAdapter(mDemoCollectionPagerAdapter);
			// mViewPager.setOnPageChangeListener(new ViewPager.SimpleOnPageChangeListener()
			// {
			// @Override
			// public void onPageSelected(int position)
			// {
			// getActionBar().setSelectedNavigationItem(position);
			// }
			// });

			// final ActionBar actionBar = getActionBar();

			// Specify that tabs should be displayed in the action bar.
			// actionBar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);

			// Create a tab listener that is called when the user changes tabs.
			// ActionBar.TabListener tabListener = new ActionBar.TabListener()
			// {
			// @Override
			// public void onTabSelected(Tab tab, FragmentTransaction ft)
			// {
			// mViewPager.setCurrentItem(tab.getPosition());
			// }
			//
			// @Override
			// public void onTabUnselected(Tab tab, FragmentTransaction ft)
			// {
			// // TODO Auto-generated method stub
			// }
			//
			// @Override
			// public void onTabReselected(Tab tab, FragmentTransaction ft)
			// {
			// // TODO Auto-generated method stub
			// }
			// };
			//
			// // Add 3 tabs, specifying the tab's text and TabListener
			// for (int i = 0; i < mDemoCollectionPagerAdapter.getCount(); i++)
			// {
			// CharSequence title = mDemoCollectionPagerAdapter.getPageTitle(i);
			// actionBar.addTab(actionBar.newTab().setText(title).setTabListener(tabListener));
			// }

			ActivityPreferences.registerOnSharedPreferenceChangeListener(this);

			startService(new Intent(this, NotificationService.class));
			startService(new Intent(this, BackgroundService.class));

			boolean firstStart = preferences.getBoolean(DevicePreferences.KEY_FIRST_START, true);
			if (firstStart)
			{
				preferences.edit().putBoolean(DevicePreferences.KEY_FIRST_START, false).apply();
				startActivity(new Intent(this, ActivityWelcome.class));
				finish();
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// handled
	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		try
		{
			cachedMenu = menu;
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
		return true;
	}

	@Override
	public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key)
	{
		if (DevicePreferences.KEY_SHOW_TIME_AFTER.equals(key))
		{
			if (preferences.getBoolean(DevicePreferences.KEY_SHOW_TIME_AFTER, true))
			{
				startService(new Intent(this, NotificationService.class));
			}
			else
			{
				stopService(new Intent(this, NotificationService.class));
			}
		}
	}

	// private void testSync()
	// {
	// // TODO: i18n
	// String TEXT_SYNC_PROGRESS = "Synchronizing...";
	//
	// final NotificationManager mNotifyManager = (NotificationManager)
	// getSystemService(Context.NOTIFICATION_SERVICE);
	// final Builder mBuilder = new NotificationCompat.Builder(this);
	// mBuilder.setContentTitle(TEXT_SYNC_PROGRESS).setSmallIcon(R.drawable.button_sync).setOngoing(true);
	// final int ID = 42;
	//
	// new AsyncTask<Void, Integer, String>()
	// {
	// @Override
	// protected String doInBackground(Void... par)
	// {
	// try
	// {
	// DiaryService diaryLocal = new DiaryLocalService(getContentResolver());
	// DiaryService diaryWeb = Storage.webDiary;
	//
	// int count = SyncUtils.synchronize_v2(diaryLocal, diaryWeb, new ProgressCallback()
	// {
	// @SuppressWarnings("synthetic-access")
	// @Override
	// public void update(int progress, int max)
	// {
	// publishProgress(progress, max);
	// }
	// });
	//
	// return String.format("Synced items: %d", count);
	// }
	// catch (Exception e)
	// {
	// // TODO: i18n
	// return BuildConfig.DEBUG ? e.getMessage() : "Failed to sync personal data";
	// }
	// }
	//
	// @Override
	// protected void onProgressUpdate(Integer... values)
	// {
	// mBuilder.setProgress(values[1], values[0], false);
	// int percentage = values[0] * 100 / values[1];
	// mBuilder.setContentText(String.format("%d %%", percentage));
	// mNotifyManager.notify(ID, mBuilder.build());
	// }
	//
	// @Override
	// protected void onPostExecute(String message)
	// {
	// if (message != null)
	// {
	// mBuilder.setContentTitle("Data sync");
	// mBuilder.setContentText(message);
	// mBuilder.setOngoing(false);
	// mNotifyManager.notify(ID, mBuilder.build());
	// }
	// else
	// {
	// mNotifyManager.cancel(ID);
	// }
	// };
	// }.execute();
	// }

	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		switch (item.getItemId())
		{
			case R.id.item_common_login:
			{
				final Intent intent = new Intent(this, ActivityLogin.class);
				intent.putExtra(ActivityLogin.ARG_ACCOUNT_TYPE, "diacomp.org");
				intent.putExtra(ActivityLogin.ARG_AUTH_TYPE, (String) null);
				intent.putExtra(ActivityLogin.ARG_IS_ADDING_NEW_ACCOUNT, true);
				// intent.putExtra(AccountManager.KEY_ACCOUNT_AUTHENTICATOR_RESPONSE, response);
				startActivityForResult(intent, CODE_LOGIN);
				return true;
			}
			case R.id.item_diary_preferences:
			{
				Intent settingsActivity = new Intent(getBaseContext(), ActivityPreferences.class);
				startActivity(settingsActivity);
				return true;
			}
			default:
			{
				return false;
			}
		}
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent intent)
	{
		super.onActivityResult(requestCode, resultCode, intent);

		switch (requestCode)
		{
			case CODE_LOGIN:
			{
				if (resultCode == Activity.RESULT_OK)
				{
					// though menu checks account every time it renders, this allows to hide login
					// icon immediately
					if (cachedMenu != null)
					{
						MenuItem item = cachedMenu.findItem(R.id.item_common_login);
						if (item != null)
						{
							item.setVisible(false);
						}
					}
				}
				break;
			}
		}
	}
}