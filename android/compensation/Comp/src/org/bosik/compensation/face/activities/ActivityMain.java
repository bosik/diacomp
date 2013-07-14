package org.bosik.compensation.face.activities;

import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;
import org.bosik.compensation.face.BuildConfig;
import org.bosik.compensation.face.R;
import org.bosik.compensation.face.UIUtils;
import org.bosik.compensation.persistence.entity.foodbase.FoodBase;
import org.bosik.compensation.persistence.repository.Storage;
import org.bosik.compensation.persistence.repository.diary.LocalDiaryRepository;
import org.bosik.compensation.persistence.repository.diary.WebDiaryRepository;
import org.bosik.compensation.persistence.repository.foodbase.FoodBaseXMLFormatter;
import org.bosik.compensation.persistence.repository.foodbase.LocalFoodBaseRepository;
import org.bosik.compensation.persistence.repository.foodbase.WebFoodBaseRepository;
import org.bosik.compensation.persistence.repository.providers.WebClient;
import org.bosik.compensation.persistence.repository.providers.WebClient.AuthException;
import org.bosik.compensation.persistence.repository.providers.WebClient.DeprecatedAPIException;
import org.bosik.compensation.persistence.repository.providers.WebClient.LoginResult;
import org.bosik.compensation.persistence.repository.providers.WebClient.NoConnectionException;
import org.bosik.compensation.persistence.repository.providers.WebClient.ResponseFormatException;
import org.bosik.compensation.persistence.repository.providers.WebClient.UndefinedFieldException;
import org.bosik.compensation.persistence.sync.SyncBaseRepository;
import org.bosik.compensation.persistence.sync.SyncDiaryRepository;
import org.bosik.compensation.persistence.sync.SyncDiaryRepository.Callback;
import org.bosik.compensation.utils.Utils;
import android.app.Activity;
import android.app.ProgressDialog;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

public class ActivityMain extends Activity implements OnSharedPreferenceChangeListener, OnClickListener
{	
	/* =========================== КОНСТАНТЫ ================================ */
	
	private static final String TAG = "ActivityMain";
	//private static final int RESULT_SPEECH_TO_TEXT = 620;

	/* =========================== ПОЛЯ ================================ */
	
	// настройки
	private SharedPreferences pref;

	// константы настроек
	private String PREF_SERVER;
	private String PREF_USERNAME;
	private String PREF_PASSWORD;
	private String PREF_DEFAULT_SERVER;
	private String PREF_DEFAULT_USERNAME;
	private String PREF_DEFAULT_PASSWORD;

	// компоненты
	private Button buttonDiary;
	private Button buttonFoodBase;
	private Button buttonDishBase;
	private Button buttonPref;
	private Button buttonAuth;
	private Button buttonTestMealEditor;
	
	public static boolean logged = false;
	private static boolean timerSettedUp = false;

	/* =========================== КЛАССЫ ================================ */

	// TODO: вынести в отдельный модуль, отвязать

	private class SyncParams
	{
		private boolean showProgress;

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
	
	private class AsyncTaskAuthAndSync extends AsyncTask<SyncParams, Integer, LoginResult> implements Callback
	{
		// <Params, Progress, Result>
		private ProgressDialog dialog_login;
		private ProgressDialog dialog_sync;
		private int syncPagesCount;
		private SyncParams syncParams;

		// константы для управления progressbar
		private static final int COM_SHOW_AUTH = 11;
		private static final int COM_SHOW_SYNC = 12;
		private static final int COM_PROGRESS_MAX = 21;
		private static final int COM_PROGRESS_CUR = 22;

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
		
		protected LoginResult doInBackground(SyncParams... par)
		{			
			Log.i(TAG, "Sync()");
			syncPagesCount = 0;
			
			syncParams = new SyncParams(par[0]);
			
			/* АВТОРИЗАЦИЯ */
			
			if (!logged)
			{
				Log.d(TAG,
						"Not logged, trying to auth (username=" + Storage.web_client.getUsername() + ", password=" + Storage.web_client.getPassword()
								+ ")");

				publishProgress(COM_SHOW_AUTH);
				try
				{
					Storage.web_client.login();
					logged = true;
					Log.d(TAG,  "Logged OK");
				}
				catch (NoConnectionException e)		{ return LoginResult.FAIL_CONNECTION; }
				catch (ResponseFormatException e)	{ return LoginResult.FAIL_FORMAT; }
				catch (DeprecatedAPIException e)	{ return LoginResult.FAIL_APIVERSION; }
				catch (AuthException e)				{ return LoginResult.FAIL_AUTH; }
				catch (UndefinedFieldException e)	{ return LoginResult.FAIL_UNDEFIELDS; }
			}
			
			/* СИНХРОНИЗАЦИЯ */
			
			publishProgress(COM_SHOW_SYNC);
	    	try
	    	{
	    		Log.d(TAG,  "Sync...");
	    		// TODO: хранить время последней синхронизации
	    		Date since = new Date(2013-1900, 01-1, 1, 0, 0, 0); // а затем мы получаем громадный синхролист, ага
	    		syncPagesCount = SyncDiaryRepository.synchronize(Storage.local_diary, Storage.web_diary, since);
	    		
	    		// TODO: create once
	    		SyncBaseRepository<FoodBase> foodSync = new SyncBaseRepository<FoodBase>();
	    		
	    		foodSync.synchronize(Storage.local_foodbase, Storage.web_foodbase);
	    		
	    		Log.d(TAG,  "Sync done OK...");
	    		return LoginResult.DONE;
	    	}
	    	catch (NoConnectionException e)		{ logged = false; Log.e(TAG, e.getLocalizedMessage()); return LoginResult.FAIL_CONNECTION; }
			catch (ResponseFormatException e)	{ logged = false; Log.e(TAG, e.getLocalizedMessage()); return LoginResult.FAIL_FORMAT; }
			catch (DeprecatedAPIException e)	{ logged = false; Log.e(TAG, e.getLocalizedMessage()); return LoginResult.FAIL_APIVERSION; }
			catch (AuthException e)				{ logged = false; Log.e(TAG, e.getLocalizedMessage()); return LoginResult.FAIL_AUTH; }
	    	catch (UndefinedFieldException e)	{ logged = false; Log.e(TAG, e.getLocalizedMessage()); return LoginResult.FAIL_UNDEFIELDS; }
		}
		
		protected void onProgressUpdate(Integer... msg)
		{
			if (!syncParams.getShowProgress())
				return;

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
			 * case COM_PROGRESS_MAX: dialog_sync.setMax(msg[1]); break; case
			 * COM_PROGRESS_CUR: dialog_sync.setProgress(msg[1]); break;
			 */
			}
		}

		protected void onPostExecute(LoginResult result)
		{
			if (!syncParams.getShowProgress()) return;
			
			if (dialog_login.isShowing())	dialog_login.dismiss();
			if (dialog_sync.isShowing())	dialog_sync.dismiss();
			switch (result)
			{
				case FAIL_UNDEFIELDS: UIUtils.showTip(ActivityMain.this, "Ошибка авторизации: укажите адрес сервера, логин и пароль"); break;
				case FAIL_AUTH: UIUtils.showTip(ActivityMain.this, "Ошибка авторизации: неверный логин/пароль"); break;
				case FAIL_CONNECTION: UIUtils.showTip(ActivityMain.this, "Ошибка: сервер не отвечает"); break;
				case FAIL_APIVERSION: UIUtils.showTip(ActivityMain.this, "Ошибка: версия API устарела, обновите приложение"); break;
				case FAIL_FORMAT: UIUtils.showTip(ActivityMain.this, "Ошибка: неверный формат"); break;
				case DONE: 
					{
						String s = syncPagesCount > 0 ? "Синхронизация прошла успешно, передано страниц: " + String.valueOf(syncPagesCount) : "Данные уже синхронизированы";
						UIUtils.showTip(ActivityMain.this, s); break;
					}
			}
		}
		
		// implemented
		public void update_max(int max)
		{
			publishProgress(COM_PROGRESS_MAX, max);
		}

		public void update_progress(int progress)
		{
			publishProgress(COM_PROGRESS_CUR, progress);
		}
	}
	
	/* =========================== МЕТОДЫ ================================ */ 
    
    // СТАНДАРТНЫЕ
    
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		if (BuildConfig.DEBUG)
		{
			UIUtils.showTip(this, "Debug mode is on");
		}

		// ПОЛУЧЕНИЕ НАСТРОЕК

		PreferenceManager.setDefaultValues(this, R.xml.preferences, false);
		pref = PreferenceManager.getDefaultSharedPreferences(this);
		PREF_SERVER = getString(R.string.prefServer);
		PREF_USERNAME = getString(R.string.prefUsername);
		PREF_PASSWORD = getString(R.string.prefPassword);
		PREF_DEFAULT_SERVER = getString(R.string.prefDefaultServer);
		PREF_DEFAULT_USERNAME = getString(R.string.prefDefaultUsername);
		PREF_DEFAULT_PASSWORD = getString(R.string.prefDefaultPassword);
    	
    	// НАСТРОЙКА СИСТЕМЫ

 		if (null == Storage.local_diary)
 		{
 			//Log.d(TAG, "Storage.init(): local diary initialization...");
 			Storage.local_diary = new LocalDiaryRepository(getContentResolver());
 		}
 		if (null == Storage.web_client) 
 		{
 			//Log.d(TAG, "Storage.init(): web client initialization...");	
 			Storage.web_client = new WebClient(Integer.parseInt(getString(R.string.connectionTimeout)));			
 		}
 		if (null == Storage.web_diary) 
 		{
 			//Log.d(TAG, "Storage.init(): web diary initialization...");	
 			Storage.web_diary = new WebDiaryRepository(Storage.web_client);			
 		}
 		if (null == Storage.local_foodbase) 
 		{
 			//Log.d(TAG, "Storage.init(): local foodbase initialization...");	
 			Storage.local_foodbase = new LocalFoodBaseRepository("foodbase.xml", getBaseContext(), new FoodBaseXMLFormatter());			
 		}
 		if (null == Storage.web_foodbase) 
 		{
 			//Log.d(TAG, "Storage.init(): web foodbase initialization...");	
 			Storage.web_foodbase = new WebFoodBaseRepository(Storage.web_client);			
 		}

 		// TODO: код, применяющий настройки, дублируется здесь и в OnPreferenceChangeListener. Что делать?
        Storage.web_client.setServer(pref.getString(PREF_SERVER,   PREF_DEFAULT_SERVER));
        Storage.web_client.setUsername(pref.getString(PREF_USERNAME, PREF_DEFAULT_USERNAME));
        Storage.web_client.setPassword(pref.getString(PREF_PASSWORD, PREF_DEFAULT_PASSWORD));
        
        // НАСТРОЙКА ИНТЕРФЕЙСА
        
        // устанавливаем макет
        setContentView(R.layout.main_menu);

        // определяем компоненты
        buttonDiary = (Button)findViewById(R.id.ButtonDiary);
    	buttonFoodBase = (Button)findViewById(R.id.ButtonFoodBase);
    	buttonDishBase = (Button)findViewById(R.id.ButtonDishBase);
    	buttonTestMealEditor = (Button)findViewById(R.id.buttonTestMealEditor);
    	buttonPref = (Button)findViewById(R.id.ButtonPreferences);
    	buttonAuth = (Button)findViewById(R.id.buttonAuth);
        
        // назначаем обработчики        
        buttonDiary.setOnClickListener(this);
        buttonFoodBase.setOnClickListener(this);
        buttonDishBase.setOnClickListener(this);
        buttonTestMealEditor.setOnClickListener(this);
        buttonPref.setOnClickListener(this);
        buttonAuth.setOnClickListener(this);  
        
        // TODO: add force single sync on start
        
        setupSyncTimer(10*60*1000);
        
        showDiary();
    }      

    @Override
    protected void onResume()
    {
        super.onResume();        
        PreferenceManager.getDefaultSharedPreferences(this).registerOnSharedPreferenceChangeListener(this);
    }

    @Override
    protected void onPause()
    {
        super.onPause();
        PreferenceManager.getDefaultSharedPreferences(this).unregisterOnSharedPreferenceChangeListener(this);
    }
    
	public void onClick(View v)
	{
		switch (v.getId())
		{
			case R.id.ButtonDiary:
				showDiary();
				break;
			case R.id.ButtonFoodBase:
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
				showMealEditor();
				break;
		}
	}

	private void setupSyncTimer(long interval)
	{
		if (timerSettedUp) return;
		timerSettedUp = true;
		
		final SyncParams par = new SyncParams();
		//final Activity mainActivity = this;
		par.setShowProgress(false);

		TimerTask task = new TimerTask()
		{
			private Handler mHandler = new Handler(Looper.getMainLooper());

			@Override
			public void run()
			{
				mHandler.post(new Runnable()
				{
					public void run()
					{
						new AsyncTaskAuthAndSync().execute(par);
						//UIUtils.showTip(mainActivity, "Tick!");
					}
				});
			}
		};

		Timer timer = new Timer();
		timer.scheduleAtFixedRate(task, 0, interval);
	}
	
	@Override
	public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key)
	{
		applyPreference(key);			
	}
	
	private static boolean check(String testKey, String baseKey)
	{
		return testKey.isEmpty() || testKey.equals(baseKey);
	}
	
	/**
	 * Пустой ключ обновляет все возможные параметры
	 * @param key
	 */
	private void applyPreference(String key)
	{
		Log.d(TAG, "applyPreferences(): key = " + key);
		
		if (check(key, PREF_SERVER))
		{
			Storage.web_client.setServer(pref.getString(key, PREF_DEFAULT_SERVER));
			ActivityMain.logged = false;
		}
		
		if (check(key, PREF_USERNAME))
		{
			Storage.web_client.setUsername(pref.getString(key, PREF_DEFAULT_USERNAME));
			ActivityMain.logged = false;
		}
		
		if (check(key, PREF_PASSWORD))
		{
			Storage.web_client.setPassword(pref.getString(key, PREF_DEFAULT_PASSWORD));
			ActivityMain.logged = false;
		}
		
		/*if (!identified && BuildConfig.DEBUG)
		{
			throw new RuntimeException("Unhandled preference modification: key=" + key);
			// TODO: как узнавать об ошибках, произошедших у пользователя в release-mode? Email? Web?
		}*/
	}
    
    // РАБОЧИЕ: ИНТЕРФЕЙС
    
    private void showDiary()
    {
    	// TODO: константы
    	Intent intent = new Intent(this, ActivityDiary.class);    	
    	intent.putExtra("date", Utils.now());
        startActivity(intent);	
    }
    
    /*private void clearLocalDiary()
    {		
		// формируем параметры
        String mSelectionClause = DiaryProvider.COLUMN_DATE + " > ?";
        String[] mSelectionArgs = {"2014-01-01"}; 
        
        // выполняем запрос
        int count = getContentResolver().delete(
            DiaryProvider.CONTENT_URI,               
            mSelectionClause,          
            mSelectionArgs);
        
        Log.w(TAG, "Deleted records: " + count);
    }*/
    
    private void showMealEditor()
    {
        startActivity(new Intent(this, ActivityMeal.class));
    }
   
    // АЛЬФА-ТЕСТИРОВАНИЕ
    
    /*private void executeTaskAsync(final Runnable R, final long timeOut)
    {
    	boolean result;
    	
    	final Thread taskThread = new Thread()
		{  
			@Override
			public void run()
			{
				R.run();
			}
		}; 
    	
    	final Thread controlThread = new Thread()
		{  
			@Override
			public void run()
			{
				try {
					taskThread.run();
					taskThread.join(timeOut);
					if (taskThread.isAlive())
					{
						result = false;
						taskThread.interrupt();
					}
					else
					{
						result = true;
					}
				} catch (InterruptedException e) {
					e.printStackTrace();
				}				
			}
		};
		
		controlThread.run();
    }*/
}
