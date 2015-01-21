package org.bosik.diacomp.android.frontend.activities;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.bosik.diacomp.android.BuildConfig;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.utils.ErrorHandler;
import android.app.Activity;
import android.app.ProgressDialog;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

public class ActivityMain extends Activity
{
	/* =========================== CONSTANTS ================================ */

	static final String	TAG	= ActivityMain.class.getSimpleName();
	// private static final int RESULT_SPEECH_TO_TEXT = 620;

	/* =========================== FIELDS ================================ */

	// Components
	private Button		buttonDiary;
	private Button		buttonBase;
	private Button		buttonPreferences;
	private Button		buttonSync;
	private Button		buttonTest;
	private Button		buttonAnalyze;

	/* =========================== CLASSES ================================ */

	// TODO: вынести в отдельный модуль, отвязать
	// TODO: refresh diary view on productive (non-trivial) sync

	class AsyncTaskAnalyzeDiary extends AsyncTask<Void, Integer, Void>
	{
		// <Params, Progress, Result>
		private ProgressDialog	dialog;

		@Override
		protected void onPreExecute()
		{
			dialog = new ProgressDialog(ActivityMain.this);
			dialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			dialog.setCancelable(false);
			// TODO: localization
			dialog.setMessage("Analyzing...");
		}

		@Override
		protected Void doInBackground(Void... par)
		{
			publishProgress(50);
			Storage.analyzeKoofs();
			publishProgress(100);

			return null;
		}

		@Override
		protected void onPostExecute(Void result)
		{
			if (dialog.isShowing())
			{
				dialog.dismiss();
			}

			// TODO: i18n
			UIUtils.showTip(ActivityMain.this, "Model updated");
		}
	}

	/* =========================== METHODS ================================ */

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		try
		{
			if (BuildConfig.DEBUG)
			{
				UIUtils.showTip(this, "Debug mode is on");
			}

			// Core initialization
			ActivityPreferences.init(this);
			Storage.init(this, getContentResolver(), ActivityPreferences.preferences);

			// НАСТРОЙКА ИНТЕРФЕЙСА

			// устанавливаем макет
			setContentView(R.layout.activity_main);

			// определяем компоненты
			buttonDiary = (Button) findViewById(R.id.ButtonDiary);
			buttonBase = (Button) findViewById(R.id.ButtonBase);
			buttonTest = (Button) findViewById(R.id.buttonTest);
			buttonPreferences = (Button) findViewById(R.id.ButtonPreferences);
			buttonSync = (Button) findViewById(R.id.buttonSync);
			buttonAnalyze = (Button) findViewById(R.id.buttonAnalyze);

			// назначаем обработчики
			buttonDiary.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					showDiary();
				}
			});
			buttonBase.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					Intent intent = new Intent(ActivityMain.this, ActivityBase.class);
					intent.putExtra(ActivityBase.KEY_MODE, ActivityBase.VALUE_MODE_EDIT);
					startActivity(intent);
				}
			});
			buttonPreferences.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					Intent settingsActivity = new Intent(getBaseContext(), ActivityPreferences.class);
					startActivity(settingsActivity);
				}
			});
			buttonSync.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					new AsyncTask<Void, Void, Map<String, Integer>>()
					{
						final String	DIARY	= "diary";
						final String	FOOD	= "food";
						final String	DISH	= "dish";

						@Override
						protected Map<String, Integer> doInBackground(Void... arg0)
						{
							Map<String, Integer> result = new HashMap<String, Integer>();

							result.put(DIARY, Storage.syncDiary());
							result.put(FOOD, Storage.syncFoodbase());
							result.put(DISH, Storage.syncDishbase());

							return result;
						}

						@Override
						protected void onPostExecute(Map<String, Integer> result)
						{
							String message = "";

							Integer countDiary = result.get(DIARY);
							if (countDiary == null)
							{
								message += "Diary: failed\n";
							}
							else if (countDiary > 0)
							{
								message += String.format("Diary: %d\n", countDiary);
							}
							else
							{
								message += "Diary: \t\tno changes\n";
							}

							// =================================================================

							Integer countFood = result.get(FOOD);
							if (countFood == null)
							{
								message += "Foods: failed\n";
							}
							else if (countFood > 0)
							{
								message += String.format("Foods: %d\n", countFood);
							}
							else
							{
								message += "Foods: \tno changes\n";
							}

							// =================================================================

							Integer countDish = result.get(DISH);
							if (countDish == null)
							{
								message += "Dishes: failed";
							}
							else if (countDish > 0)
							{
								message += String.format("Dishes: %d", countDish);
							}
							else
							{
								message += "Dishes:\tno changes";
							}

							UIUtils.showTip(ActivityMain.this, message);
						}
					}.execute();
				}
			});
			buttonTest.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					// HardcodedFoodbase.restoreHardcodedBase();
					// UIUtils.showTip(ActivityMain.this, "Hardcoded base restored");

					Intent intent = new Intent(ActivityMain.this, ActivityGraph.class);
					startActivity(intent);

					// DiaryService diary = new DiaryLocalService(getContentResolver());
					// HashUtils.updateHashTree(diary, "");
				}
			});
			buttonAnalyze.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					// Storage.analyzeKoofs();
					ActivityMain.AsyncTaskAnalyzeDiary task = new ActivityMain.AsyncTaskAnalyzeDiary();
					task.execute();
				}
			});

			showDiary();
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	void showDiary()
	{
		Intent intent = new Intent(this, ActivityDiary.class);
		intent.putExtra(ActivityDiary.KEY_DATE, new Date());
		startActivity(intent);
	}
}