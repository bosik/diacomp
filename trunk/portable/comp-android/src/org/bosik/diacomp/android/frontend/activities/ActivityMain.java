package org.bosik.diacomp.android.frontend.activities;

import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.android.BuildConfig;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.fragments.FragmentBase;
import org.bosik.diacomp.android.frontend.fragments.FragmentDiaryScroller;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.sync.HashUtils;
import org.bosik.diacomp.core.services.sync.SyncUtils;
import org.bosik.diacomp.core.services.sync.SyncUtils.ProgressCallback;
import org.bosik.diacomp.core.utils.Utils;
import android.accounts.Account;
import android.accounts.AccountManager;
import android.app.NotificationManager;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.app.NotificationCompat;
import android.support.v4.app.NotificationCompat.Builder;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;

public class ActivityMain extends FragmentActivity
{
	/* =========================== CONSTANTS ================================ */

	static final String			TAG			= ActivityMain.class.getSimpleName();
	// private static final int RESULT_SPEECH_TO_TEXT = 620;

	private static final int	TAB_COUNT	= 2;
	private static final int	TAB_DIARY	= 0;
	private static final int	TAB_BASE	= 1;

	/* =========================== FIELDS ================================ */

	ViewPager					mViewPager;

	/* =========================== METHODS ================================ */

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		try
		{
			// Backend

			if (BuildConfig.DEBUG)
			{
				UIUtils.showTip(this, "Debug mode is on");
			}

			PreferenceManager.setDefaultValues(this, R.xml.preferences, false);
			Storage.init(this, getContentResolver(), PreferenceManager.getDefaultSharedPreferences(this));

			// Account sync
			AccountManager am = AccountManager.get(this);
			Account[] accounts = am.getAccountsByType("diacomp.org");
			if (accounts.length > 0)
			{
				long SYNC_INTERVAL = 60; // sec
				ContentResolver.setIsSyncable(accounts[0], DiaryContentProvider.AUTHORITY, 1);
				ContentResolver.setSyncAutomatically(accounts[0], DiaryContentProvider.AUTHORITY, true);
				ContentResolver.addPeriodicSync(accounts[0], DiaryContentProvider.AUTHORITY, Bundle.EMPTY,
						SYNC_INTERVAL);
			}
			else
			{
				Log.w(TAG, "No account found");
			}

			// Frontend

			setContentView(R.layout.activity_main);
			FragmentStatePagerAdapter mDemoCollectionPagerAdapter = new FragmentStatePagerAdapter(
					getSupportFragmentManager())
			{
				@Override
				public int getCount()
				{
					return TAB_COUNT;
				}

				@Override
				public Fragment getItem(int position)
				{
					switch (position)
					{
						case TAB_DIARY:
						{
							return new FragmentDiaryScroller();
						}
						case TAB_BASE:
						{
							return new FragmentBase();
						}
						default:
						{
							throw new IllegalArgumentException(String.format("Index %d is out of bounds", position));
						}
					}
				}

				@Override
				public CharSequence getPageTitle(int position)
				{
					switch (position)
					{
						case TAB_DIARY:
						{
							return ActivityMain.this.getString(R.string.main_option_diary);
						}
						case TAB_BASE:
						{
							return ActivityMain.this.getString(R.string.main_option_bases);
						}
						default:
						{
							throw new IllegalArgumentException(String.format("Index %d is out of bounds", position));
						}
					}
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
			MenuInflater inflater = getMenuInflater();
			inflater.inflate(R.menu.actions_common, menu);
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
		return true;
	}

	public void testSyncPerformance()
	{
		long time = System.currentTimeMillis();
		SortedMap<String, String> data = new TreeMap<String, String>();
		for (int i = 0; i < 25000; i++)
		{
			String id = Utils.generateGuid();
			String hash = Utils.generateGuid();
			data.put(id, hash);
		}
		time = System.currentTimeMillis() - time;
		Log.i(TAG, String.format("%d items prepared in %d ms", data.size(), time));

		time = System.currentTimeMillis();
		SortedMap<String, String> tree = HashUtils.buildHashTree(data);
		time = System.currentTimeMillis() - time;

		Log.i(TAG, String.format("Tree with %d items build in %d ms", tree.size(), time));
	}

	private void testSync()
	{
		// TODO: i18n
		String TEXT_SYNC_PROGRESS = "Synchronizing...";

		final NotificationManager mNotifyManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
		final Builder mBuilder = new NotificationCompat.Builder(this);
		mBuilder.setContentTitle(TEXT_SYNC_PROGRESS).setSmallIcon(R.drawable.button_sync).setOngoing(true);
		final int ID = 42;

		new AsyncTask<Void, Integer, String>()
		{
			@Override
			protected String doInBackground(Void... par)
			{
				try
				{
					DiaryService diaryLocal = new DiaryLocalService(getContentResolver());
					DiaryService diaryWeb = Storage.webDiary;

					int count = SyncUtils.synchronize_v2(diaryLocal, diaryWeb, new ProgressCallback()
					{
						@SuppressWarnings("synthetic-access")
						@Override
						public void update(int progress, int max)
						{
							publishProgress(progress, max);
						}
					});

					return String.format("Synced items: %d", count);
				}
				// catch (ConnectTimeoutException e)
				// {
				// return "Server not responding";
				// }
				catch (Exception e)
				{
					// TODO: i18n
					return BuildConfig.DEBUG ? e.getMessage() : "Failed to sync personal data";
				}
			}

			@Override
			protected void onProgressUpdate(Integer... values)
			{
				mBuilder.setProgress(values[1], values[0], false);
				int percentage = values[0] * 100 / values[1];
				mBuilder.setContentText(String.format("%d %%", percentage));
				mNotifyManager.notify(ID, mBuilder.build());
			}

			@Override
			protected void onPostExecute(String message)
			{
				if (message != null)
				{
					mBuilder.setContentTitle("Data sync");
					mBuilder.setContentText(message);
					mBuilder.setOngoing(false);
					mNotifyManager.notify(ID, mBuilder.build());
				}
				else
				{
					mNotifyManager.cancel(ID);
				}
			};
		}.execute();
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		switch (item.getItemId())
		{
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
}