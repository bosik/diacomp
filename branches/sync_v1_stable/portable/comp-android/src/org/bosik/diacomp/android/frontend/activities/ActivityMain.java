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

	static final String			TAG									= ActivityMain.class.getSimpleName();
	// private static final int RESULT_SPEECH_TO_TEXT = 620;

	private static final String	MESSAGE_PROGRESS_AUTH				= "Авторизация...";
	private static final String	MESSAGE_PROGRESS_SYNC				= "Синхронизация...";
	private static final String	MESSAGE_ERROR_BAD_CREDENTIALS		= "Ошибка авторизации: неверный логин/пароль";
	private static final String	MESSAGE_ERROR_SERVER_NOT_RESPONDING	= "Ошибка: сервер не отвечает";
	private static final String	MESSAGE_ERROR_UNDEFINED_AUTH		= "Ошибка авторизации: укажите адрес сервера, логин и пароль";
	private static final String	MESSAGE_ERROR_DEPRECATED_API		= "Ошибка: версия API устарела, обновите приложение";
	private static final String	MESSAGE_ERROR_BAD_RESPONSE			= "Ошибка: неверный формат";
	private static final String	MESSAGE_SYNCED_OK					= "Синхронизация дневника прошла успешно, передано записей: %d";
	private static final String	MESSAGE_FOODBASE_SYNCED_OK			= "База продуктов синхронизирована";
	private static final String	MESSAGE_NO_UPDATES					= "Обновлений нет";

	/* =========================== FIELDS ================================ */

	// Components
	private Button				buttonDiary;
	private Button				buttonBase;
	private Button				buttonPreferences;
	private Button				buttonSync;
	private Button				buttonTest;
	private Button				buttonAnalyze;

	/* =========================== CLASSES ================================ */

	// TODO: вынести в отдельный модуль, отвязать
	// TODO: refresh diary view on productive (non-trivial) sync

	private class SyncParams
	{
		private boolean	showProgress;

		public SyncParams()
		{
		}

		public SyncParams(SyncParams origin)
		{
			setShowProgress(origin.getShowProgress());
		}

		public boolean getShowProgress()
		{
			return showProgress;
		}

		public void setShowProgress(boolean showProgress)
		{
			this.showProgress = showProgress;
		}
	}

	@Deprecated
	public enum LoginResult
	{
		/**
		 * Одно из полей не определено
		 */
		FAIL_UNDEFIELDS,

		/**
		 * Сервер не отвечает
		 */
		FAIL_CONNECTION,

		/**
		 * Сервер отвечает некорректно
		 */
		FAIL_FORMAT,

		/**
		 * Сервер сообщает об устаревшей версии API
		 */
		FAIL_APIVERSION,

		/**
		 * Сервер сообщает о неправильной паре "логин-пароль"
		 */
		FAIL_AUTH,

		/**
		 * Сервер сообщает об успешной авторизации
		 */
		DONE
	}

	// class AsyncTaskAuthAndSync extends AsyncTask<SyncParams, Integer, LoginResult> implements
	// Callback
	// {
	// // <Params, Progress, Result>
	// private ProgressDialog dialog_login;
	// private ProgressDialog dialog_sync;
	// private int syncDiaryItemsCount;
	// private int syncFoodItemsCount;
	// private SyncParams syncParams;
	//
	// // константы для управления progressbar
	// private static final int COM_SHOW_AUTH = 11;
	// private static final int COM_SHOW_SYNC = 12;
	// private static final int COM_PROGRESS_MAX = 21;
	// private static final int COM_PROGRESS_CUR = 22;
	//
	// @Override
	// protected void onPreExecute()
	// {
	// dialog_login = new ProgressDialog(ActivityMain.this);
	// dialog_login.setProgressStyle(ProgressDialog.STYLE_SPINNER);
	// dialog_login.setCancelable(false);
	// dialog_login.setMessage(MESSAGE_PROGRESS_AUTH);
	//
	// dialog_sync = new ProgressDialog(ActivityMain.this);
	// dialog_sync.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
	// dialog_sync.setCancelable(false);
	// dialog_sync.setMessage(MESSAGE_PROGRESS_SYNC);
	// }
	//
	// @Override
	// protected LoginResult doInBackground(SyncParams... par)
	// {
	// Log.i(TAG, "Sync()");
	// syncDiaryItemsCount = 0;
	// syncFoodItemsCount = 0;
	//
	// syncParams = new SyncParams(par[0]);
	//
	// /* AUTH */
	//
	// // if (!Storage.webClient.isOnline())
	// // {
	// // Log.d(TAG, "Not logged, trying to auth (username=" + Storage.webClient.getUsername()
	// // + ", password="
	// // + Storage.webClient.getPassword() + ")");
	// //
	// // publishProgress(COM_SHOW_AUTH);
	// // try
	// // {
	// // Storage.webClient.login();
	// // Log.d(TAG, "Logged OK");
	// // }
	// // catch (ConnectionException e)
	// // {
	// // return LoginResult.FAIL_CONNECTION;
	// // }
	// // catch (ResponseFormatException e)
	// // {
	// // return LoginResult.FAIL_FORMAT;
	// // }
	// // catch (DeprecatedAPIException e)
	// // {
	// // return LoginResult.FAIL_APIVERSION;
	// // }
	// // catch (AuthException e)
	// // {
	// // return LoginResult.FAIL_AUTH;
	// // }
	// // catch (UndefinedFieldException e)
	// // {
	// // return LoginResult.FAIL_UNDEFIELDS;
	// // }
	// // }
	//
	// /* SYNC */
	//
	// publishProgress(COM_SHOW_SYNC);
	// try
	// {
	// Log.v(TAG, "Sync diary...");
	// // TODO: store last sync time
	// Date since = Utils.time(2013, 11, 1, 0, 0, 0);
	// syncDiaryItemsCount = SyncService.synchronize(Storage.localDiary, Storage.webDiary, since);
	// Log.v(TAG, "Diary synced, total tranferred: " + syncDiaryItemsCount);
	//
	// Log.v(TAG, "Sync foodbase...");
	// syncFoodItemsCount = SyncService.synchronize(Storage.localFoodBase, Storage.webFoodBase,
	// since);
	// Log.v(TAG, "Foodbase synced, total tranferred: " + syncFoodItemsCount);
	//
	// Log.v(TAG, "Sync done OK");
	// return LoginResult.DONE;
	// }
	// catch (ConnectionException e)
	// {
	// // Storage.logged = false;
	// Log.e(TAG, e.getLocalizedMessage());
	// return LoginResult.FAIL_CONNECTION;
	// }
	// catch (ResponseFormatException e)
	// {
	// // Storage.logged = false;
	// Log.e(TAG, e.getLocalizedMessage());
	// return LoginResult.FAIL_FORMAT;
	// }
	// catch (DeprecatedAPIException e)
	// {
	// // Storage.logged = false;
	// Log.e(TAG, e.getLocalizedMessage());
	// return LoginResult.FAIL_APIVERSION;
	// }
	// catch (AuthException e)
	// {
	// // Storage.logged = false;
	// Log.e(TAG, e.getLocalizedMessage());
	// return LoginResult.FAIL_AUTH;
	// }
	// catch (UndefinedFieldException e)
	// {
	// // Storage.logged = false;
	// Log.e(TAG, e.getLocalizedMessage());
	// return LoginResult.FAIL_UNDEFIELDS;
	// }
	// }
	//
	// @Override
	// protected void onProgressUpdate(Integer... msg)
	// {
	// if (!syncParams.getShowProgress())
	// {
	// return;
	// }
	//
	// switch (msg[0])
	// {
	// case COM_SHOW_AUTH:
	// dialog_login.show();
	// break;
	// case COM_SHOW_SYNC:
	// dialog_login.dismiss();
	// dialog_sync.show();
	// break;
	// /*
	// * case COM_PROGRESS_MAX: dialog_sync.setMax(msg[1]); break; case COM_PROGRESS_CUR:
	// * dialog_sync.setProgress(msg[1]); break;
	// */
	// }
	// }
	//
	// @Override
	// protected void onPostExecute(LoginResult result)
	// {
	// if (syncParams.getShowProgress())
	// {
	// if (dialog_login.isShowing())
	// {
	// dialog_login.dismiss();
	// }
	// if (dialog_sync.isShowing())
	// {
	// dialog_sync.dismiss();
	// }
	// }
	//
	// switch (result)
	// {
	// case FAIL_UNDEFIELDS:
	// if (syncParams.getShowProgress())
	// {
	// UIUtils.showTip(ActivityMain.this, MESSAGE_ERROR_UNDEFINED_AUTH);
	// }
	// break;
	// case FAIL_AUTH:
	// if (syncParams.getShowProgress())
	// {
	// UIUtils.showTip(ActivityMain.this, MESSAGE_ERROR_BAD_CREDENTIALS);
	// }
	// break;
	// case FAIL_CONNECTION:
	// if (syncParams.getShowProgress())
	// {
	// UIUtils.showTip(ActivityMain.this, MESSAGE_ERROR_SERVER_NOT_RESPONDING);
	// }
	// break;
	// case FAIL_APIVERSION:
	// if (syncParams.getShowProgress())
	// {
	// UIUtils.showTip(ActivityMain.this, MESSAGE_ERROR_DEPRECATED_API);
	// }
	// break;
	// case FAIL_FORMAT:
	// if (syncParams.getShowProgress())
	// {
	// UIUtils.showTip(ActivityMain.this, MESSAGE_ERROR_BAD_RESPONSE);
	// }
	// break;
	// case DONE:
	// {
	// String s;
	//
	// boolean transferred = false;
	//
	// // check diary
	// if (syncDiaryItemsCount > 0)
	// {
	// transferred = true;
	// s = String.format(Locale.US, MESSAGE_SYNCED_OK, syncDiaryItemsCount);
	// UIUtils.showTip(ActivityMain.this, s);
	// }
	//
	// // check foodbase
	// if (syncFoodItemsCount > 0)
	// {
	// transferred = true;
	// UIUtils.showTip(ActivityMain.this, MESSAGE_FOODBASE_SYNCED_OK);
	// }
	//
	// // check nothing happen
	// if (!transferred && syncParams.getShowProgress())
	// {
	// UIUtils.showTip(ActivityMain.this, MESSAGE_NO_UPDATES);
	// }
	//
	// break;
	// }
	// }
	// }
	//
	// // implemented
	// @Override
	// public void update_max(int max)
	// {
	// publishProgress(COM_PROGRESS_MAX, max);
	// }
	//
	// @Override
	// public void update_progress(int progress)
	// {
	// publishProgress(COM_PROGRESS_CUR, progress);
	// }
	// }

	/* =========================== МЕТОДЫ ================================ */

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

	// СТАНДАРТНЫЕ

	// handled
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

	// РАБОЧИЕ: ИНТЕРФЕЙС

	void showDiary()
	{
		// TODO: константы
		Intent intent = new Intent(this, ActivityDiary.class);
		intent.putExtra("date", new Date());
		startActivity(intent);
	}

	/*
	 * private void clearLocalDiary() { // формируем параметры String mSelectionClause =
	 * DiaryContentProvider.COLUMN_DIARY_DATE + " > ?"; String[] mSelectionArgs = {"2014-01-01"};
	 * 
	 * // выполняем запрос int count = getContentResolver().delete(
	 * DiaryContentProvider.CONTENT_DIARY_URI, mSelectionClause, mSelectionArgs);
	 * 
	 * Log.w(TAG, "Deleted records: " + count); }
	 */

	// АЛЬФА-ТЕСТИРОВАНИЕ

	/*
	 * private void executeTaskAsync(final Runnable R, final long timeOut) { boolean result;
	 * 
	 * final Thread taskThread = new Thread() {
	 * 
	 * @Override public void run() { R.run(); } };
	 * 
	 * final Thread controlThread = new Thread() {
	 * 
	 * @Override public void run() { try { taskThread.run(); taskThread.join(timeOut); if
	 * (taskThread.isAlive()) { result = false; taskThread.interrupt(); } else { result = true; } }
	 * catch (InterruptedException e) { e.printStackTrace(); } } };
	 * 
	 * controlThread.run(); }
	 */
}
