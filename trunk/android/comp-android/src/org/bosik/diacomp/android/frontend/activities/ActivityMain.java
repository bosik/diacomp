package org.bosik.diacomp.android.frontend.activities;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.android.BuildConfig;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.HardcodedFoodbase;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.utils.ErrorHandler;
import android.app.Activity;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

public class ActivityMain extends Activity implements OnClickListener
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
	private Button				buttonFoodBase;
	private Button				buttonPref;
	private Button				buttonAuth;
	private Button				buttonTestMealEditor;

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
			buttonFoodBase = (Button) findViewById(R.id.ButtonBases);
			buttonTestMealEditor = (Button) findViewById(R.id.buttonTestMealEditor);
			buttonPref = (Button) findViewById(R.id.ButtonPreferences);
			buttonAuth = (Button) findViewById(R.id.buttonAuth);

			// назначаем обработчики
			buttonDiary.setOnClickListener(this);
			buttonFoodBase.setOnClickListener(this);
			buttonTestMealEditor.setOnClickListener(this);
			buttonPref.setOnClickListener(this);
			buttonAuth.setOnClickListener(this);

			showDiary();
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// handled
	@Override
	public void onClick(View v)
	{
		try
		{
			switch (v.getId())
			{
				case R.id.ButtonDiary:
					showDiary();
					break;
				case R.id.ButtonBases:
					Intent intent = new Intent(this, ActivityBase.class);
					intent.putExtra(ActivityBase.KEY_MODE, ActivityBase.VALUE_MODE_EDIT);
					startActivity(intent);
					break;
				case R.id.ButtonPreferences:
					Intent settingsActivity = new Intent(getBaseContext(), ActivityPreferences.class);
					startActivity(settingsActivity);
					break;
				case R.id.buttonAuth:

					new AsyncTask<Void, Void, List<Boolean>>()
					{
						@Override
						protected List<Boolean> doInBackground(Void... arg0)
						{
							List<Boolean> result = new ArrayList<Boolean>(3);

							result.add(Storage.syncDiary());
							result.add(Storage.syncFoodbase());
							result.add(Storage.syncDishbase());

							return result;
						}

						@Override
						protected void onPostExecute(List<Boolean> result)
						{
							UIUtils.showTip(ActivityMain.this, result.get(0) ? "Diary synced" : "Failed to sync diary");
							UIUtils.showTip(ActivityMain.this, result.get(1) ? "Foodbase synced"
									: "Failed to sync foodbase");
							UIUtils.showTip(ActivityMain.this, result.get(2) ? "Dishbase synced"
									: "Failed to sync dishbase");
						}
					}.execute();

					// if (ok)
					// {
					// UIUtils.showTip(this, "Synced ok");
					// }
					// else
					// {
					// UIUtils.showTip(this, "Failed to sync");
					// }
					break;
				case R.id.buttonTestMealEditor:
					HardcodedFoodbase.restoreHardcodedBase();
					UIUtils.showTip(this, "Hardcoded base restored");
					break;
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	// РАБОЧИЕ: ИНТЕРФЕЙС

	private void showDiary()
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