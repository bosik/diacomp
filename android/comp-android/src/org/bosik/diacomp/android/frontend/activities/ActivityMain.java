package org.bosik.diacomp.android.frontend.activities;

import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;
import org.bosik.diacomp.android.BuildConfig;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.backend.common.SyncBaseService;
import org.bosik.diacomp.android.backend.common.SyncBaseService.SyncResult;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.AuthException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.ConnectionException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.ResponseFormatException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.UndefinedFieldException;
import org.bosik.diacomp.android.backend.features.sync.SyncService;
import org.bosik.diacomp.android.backend.features.sync.SyncService.Callback;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.utils.ErrorHandler;
import android.app.Activity;
import android.app.ProgressDialog;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

public class ActivityMain extends Activity implements OnClickListener
{
	/* =========================== CONSTANTS ================================ */

	private static final String	TAG						= ActivityMain.class.getSimpleName();
	// private static final int RESULT_SPEECH_TO_TEXT = 620;

	/* =========================== FIELDS ================================ */

	// Components
	private Button				buttonDiary;
	private Button				buttonFoodBase;
	private Button				buttonDishBase;
	private Button				buttonPref;
	private Button				buttonAuth;
	private Button				buttonTestMealEditor;

	private static boolean		timerSettedUp			= false;

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

	/**
	 * Результат авторизации
	 *
	 * @author Bosik
	 */
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

	private class AsyncTaskAuthAndSync extends AsyncTask<SyncParams, Integer, LoginResult> implements Callback
	{
		// <Params, Progress, Result>
		private ProgressDialog		dialog_login;
		private ProgressDialog		dialog_sync;
		private int					syncPagesCount;
		private boolean				syncFoodBase;
		private SyncParams			syncParams;

		// константы для управления progressbar
		private static final int	COM_SHOW_AUTH		= 11;
		private static final int	COM_SHOW_SYNC		= 12;
		private static final int	COM_PROGRESS_MAX	= 21;
		private static final int	COM_PROGRESS_CUR	= 22;

		@Override
		protected void onPreExecute()
		{
			dialog_login = new ProgressDialog(ActivityMain.this);
			dialog_login.setProgressStyle(ProgressDialog.STYLE_SPINNER);
			dialog_login.setCancelable(false);
			dialog_login.setMessage("Авторизация...");

			dialog_sync = new ProgressDialog(ActivityMain.this);
			dialog_sync.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			dialog_sync.setCancelable(false);
			dialog_sync.setMessage("Синхронизация...");
		}

		@Override
		protected LoginResult doInBackground(SyncParams... par)
		{
			Log.i(TAG, "Sync()");
			syncPagesCount = 0;

			syncParams = new SyncParams(par[0]);

			/* АВТОРИЗАЦИЯ */

			if (!Storage.webClient.isOnline())
			{
				Log.d(TAG, "Not logged, trying to auth (username=" + Storage.webClient.getUsername() + ", password="
						+ Storage.webClient.getPassword() + ")");

				publishProgress(COM_SHOW_AUTH);
				try
				{
					Storage.webClient.login();
					Log.d(TAG, "Logged OK");
				}
				catch (ConnectionException e)
				{
					return LoginResult.FAIL_CONNECTION;
				}
				catch (ResponseFormatException e)
				{
					return LoginResult.FAIL_FORMAT;
				}
				catch (DeprecatedAPIException e)
				{
					return LoginResult.FAIL_APIVERSION;
				}
				catch (AuthException e)
				{
					return LoginResult.FAIL_AUTH;
				}
				catch (UndefinedFieldException e)
				{
					return LoginResult.FAIL_UNDEFIELDS;
				}
			}

			/* СИНХРОНИЗАЦИЯ */

			publishProgress(COM_SHOW_SYNC);
			try
			{
				Log.v(TAG, "Sync diary...");
				// TODO: хранить время последней синхронизации
				Date since = new Date(2013 - 1900, 11 - 1, 1, 0, 0, 0); // а затем мы получаем
																		// громадный синхролист, ага
				// TODO: restore when compiled OK
				syncPagesCount = SyncService.synchronize(Storage.localDiary, Storage.webDiary, since);
				Log.v(TAG, "Diary synced, total tranferred: " + syncPagesCount);

				Log.v(TAG, "Sync foodbase...");
				SyncResult r = SyncBaseService.synchronize(Storage.localFoodBase, Storage.webFoodBase);
				syncFoodBase = (r != SyncResult.EQUAL);
				Log.v(TAG, "Foodbase synced, result: " + r);

				Log.v(TAG, "Sync done OK");
				return LoginResult.DONE;
			}
			catch (ConnectionException e)
			{
				// Storage.logged = false;
				Log.e(TAG, e.getLocalizedMessage());
				return LoginResult.FAIL_CONNECTION;
			}
			catch (ResponseFormatException e)
			{
				// Storage.logged = false;
				Log.e(TAG, e.getLocalizedMessage());
				return LoginResult.FAIL_FORMAT;
			}
			catch (DeprecatedAPIException e)
			{
				// Storage.logged = false;
				Log.e(TAG, e.getLocalizedMessage());
				return LoginResult.FAIL_APIVERSION;
			}
			catch (AuthException e)
			{
				// Storage.logged = false;
				Log.e(TAG, e.getLocalizedMessage());
				return LoginResult.FAIL_AUTH;
			}
			catch (UndefinedFieldException e)
			{
				// Storage.logged = false;
				Log.e(TAG, e.getLocalizedMessage());
				return LoginResult.FAIL_UNDEFIELDS;
			}
		}

		@Override
		protected void onProgressUpdate(Integer... msg)
		{
			if (!syncParams.getShowProgress())
			{
				return;
			}

			switch (msg[0])
			{
				case COM_SHOW_AUTH:
					dialog_login.show();
					break;
				case COM_SHOW_SYNC:
					dialog_login.dismiss();
					dialog_sync.show();
					break;
			/*
			 * case COM_PROGRESS_MAX: dialog_sync.setMax(msg[1]); break; case COM_PROGRESS_CUR:
			 * dialog_sync.setProgress(msg[1]); break;
			 */
			}
		}

		@Override
		protected void onPostExecute(LoginResult result)
		{
			if (syncParams.getShowProgress())
			{
				if (dialog_login.isShowing())
				{
					dialog_login.dismiss();
				}
				if (dialog_sync.isShowing())
				{
					dialog_sync.dismiss();
				}
			}
			switch (result)
			{
				case FAIL_UNDEFIELDS:
					if (syncParams.getShowProgress())
					{
						UIUtils.showTip(ActivityMain.this, "Ошибка авторизации: укажите адрес сервера, логин и пароль");
					}
					break;
				case FAIL_AUTH:
					if (syncParams.getShowProgress())
					{
						UIUtils.showTip(ActivityMain.this, "Ошибка авторизации: неверный логин/пароль");
					}
					break;
				case FAIL_CONNECTION:
					if (syncParams.getShowProgress())
					{
						UIUtils.showTip(ActivityMain.this, "Ошибка: сервер не отвечает");
					}
					break;
				case FAIL_APIVERSION:
					if (syncParams.getShowProgress())
					{
						UIUtils.showTip(ActivityMain.this, "Ошибка: версия API устарела, обновите приложение");
					}
					break;
				case FAIL_FORMAT:
					if (syncParams.getShowProgress())
					{
						UIUtils.showTip(ActivityMain.this, "Ошибка: неверный формат");
					}
					break;
				case DONE:
				{
					String s;

					boolean transferred = false;

					// check diary
					if (syncPagesCount > 0)
					{
						transferred = true;
						s = "Синхронизация дневника прошла успешно, передано страниц: "
								+ String.valueOf(syncPagesCount);
						UIUtils.showTip(ActivityMain.this, s);
					}

					// check foodbase
					if (syncFoodBase)
					{
						transferred = true;
						UIUtils.showTip(ActivityMain.this, "База продуктов синхронизирована");
					}

					// check nothing happen
					if (!transferred && syncParams.getShowProgress())
					{
						s = "Обновлений нет";
						UIUtils.showTip(ActivityMain.this, s);
					}

					break;
				}
			}
		}

		// implemented
		@Override
		public void update_max(int max)
		{
			publishProgress(COM_PROGRESS_MAX, max);
		}

		@Override
		public void update_progress(int progress)
		{
			publishProgress(COM_PROGRESS_CUR, progress);
		}
	}

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
			setContentView(R.layout.main_menu);

			// определяем компоненты
			buttonDiary = (Button) findViewById(R.id.ButtonDiary);
			buttonFoodBase = (Button) findViewById(R.id.ButtonFoodBase);
			buttonDishBase = (Button) findViewById(R.id.ButtonDishBase);
			buttonTestMealEditor = (Button) findViewById(R.id.buttonTestMealEditor);
			buttonPref = (Button) findViewById(R.id.ButtonPreferences);
			buttonAuth = (Button) findViewById(R.id.buttonAuth);

			// назначаем обработчики
			buttonDiary.setOnClickListener(this);
			buttonFoodBase.setOnClickListener(this);
			buttonDishBase.setOnClickListener(this);
			buttonTestMealEditor.setOnClickListener(this);
			buttonPref.setOnClickListener(this);
			buttonAuth.setOnClickListener(this);

			// TODO: add force single sync on start

			// setupSyncTimer(10 * 60 * 1000);

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
				case R.id.ButtonFoodBase:
					Intent intent = new Intent(this, ActivityFoodbase.class);
					intent.putExtra(ActivityFoodbase.KEY_MODE, ActivityFoodbase.VALUE_MODE_EDIT);
					startActivity(intent);
					break;
				case R.id.ButtonDishBase:
					break;
				case R.id.ButtonPreferences:
					Intent settingsActivity = new Intent(getBaseContext(), ActivityPreferences.class);
					startActivity(settingsActivity);
					break;
				case R.id.buttonAuth:
					SyncParams par = new SyncParams();
					par.setShowProgress(true);
					new AsyncTaskAuthAndSync().execute(par);
					break;
				case R.id.buttonTestMealEditor:
					throw new RuntimeException("Test exception");
					// break;
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	private void setupSyncTimer(long interval)
	{
		if (timerSettedUp)
		{
			return;
		}
		timerSettedUp = true;

		final SyncParams par = new SyncParams();
		par.setShowProgress(false);

		TimerTask task = new TimerTask()
		{
			private final Handler	mHandler	= new Handler(Looper.getMainLooper());

			@Override
			public void run()
			{
				mHandler.post(new Runnable()
				{
					@Override
					public void run()
					{
						new AsyncTaskAuthAndSync().execute(par);
						// UIUtils.showTip(mainActivity, "Tick!");
					}
				});
			}
		};

		Timer timer = new Timer();
		timer.scheduleAtFixedRate(task, 0, interval);
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
